const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const lx = @import("zlox.zig");
const GCopts = @import("GCopts");

const GreyStack = std.ArrayList(*lx.Obj);

inner_alloc: Allocator,
vm: *lx.VM = undefined,
bytesAllocated: usize = 0,
nextGC: usize = 1024 * 1024,
greyStack: GreyStack,
strings: lx.Table(void),

pub const GC = @This();

pub fn init(s: *GC, inner_alloc: Allocator) void {
    s.* = .{
        .greyStack = GreyStack.init(inner_alloc),
        .inner_alloc = inner_alloc,
        .strings = lx.Table(void).init(s.allocator()),
    };
}

pub fn deinit(s: *GC) void {
    s.greyStack.deinit();
    s.strings.deinit();
}

//s.strings.deinit();
pub fn allocator(s: *GC) Allocator {
    return .{
        .ptr = s,
        .vtable = &.{
            .alloc = alloc,
            .free = free,
            .resize = resize,
        },
    };
}

/// Attempt to allocate exactly `len` bytes aligned to `1 << ptr_align`.
///
/// `ret_addr` is optionally provided as the first return address of the
/// allocation call stack. If the value is `0` it means no return address
/// has been provided.
fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    const s: *GC = @ptrCast(@alignCast(ctx));
    const new = s.inner_alloc.rawAlloc(len, ptr_align, ret_addr);

    if (new) |_| {
        s.bytesAllocated += len;
        s.maybeCollect();
    }

    return new;
}

/// Attempt to expand or shrink memory in place. `buf.len` must equal the
/// length requested from the most recent successful call to `alloc` or
/// `resize`. `buf_align` must equal the same value that was passed as the
/// `ptr_align` parameter to the original `alloc` call.
///
/// A result of `true` indicates the resize was successful and the
/// allocation now has the same address but a size of `new_len`. `false`
/// indicates the resize could not be completed without moving the
/// allocation to a different address.
///
/// `new_len` must be greater than zero.
///
/// `ret_addr` is optionally provided as the first return address of the
/// allocation call stack. If the value is `0` it means no return address
/// has been provided.
fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
    const s: *GC = @ptrCast(@alignCast(ctx));
    const success = s.inner_alloc.rawResize(buf, buf_align, new_len, ret_addr);
    if (success) {
        s.bytesAllocated += new_len - buf.len;
        if (new_len > buf.len) {
            s.maybeCollect();
        }
    }
    return success;
}

/// Free and invalidate a buffer.
///
/// `buf.len` must equal the most recent length returned by `alloc` or
/// given to a successful `resize` call.
///
/// `buf_align` must equal the same value that was passed as the
/// `ptr_align` parameter to the original `alloc` call.
///
/// `ret_addr` is optionally provided as the first return address of the
/// allocation call stack. If the value is `0` it means no return address
/// has been provided.
fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
    const s: *GC = @ptrCast(@alignCast(ctx));
    s.inner_alloc.rawFree(buf, buf_align, ret_addr);
    s.bytesAllocated -= buf.len;
}

pub fn create(s: *GC, comptime O: type, args: anytype) !*O {
    var p = try s.allocator().create(O);

    if (GCopts.debug)
        std.debug.print("alloc {s: >20}@{x}\n", .{ @typeName(O), @intFromPtr(p) });

    // the only reason the init functions return the pointer instead of mutating it
    // is so we can intern strings
    if (try O.init(p, s, args)) |old| {
        return old; // returned an existing object so just return it
    } else {
        // returned same ptr so register new object
        p.obj = lx.Obj{
            .tp = lx.Obj.cast(O),
            .next = s.vm.objects,
        };
        s.vm.objects = &p.obj;
    }
    return p;
}

pub fn maybeCollect(s: *GC) void {
    if (GCopts.stress or (s.bytesAllocated > s.nextGC)) {
        s.garbageCollect();
    }
}

pub fn garbageCollect(s: *GC) void {
    if (GCopts.debug) std.debug.print("-- gc begin\n", .{});
    const before = s.bytesAllocated;

    s.markRoots(s.vm);
    s.traceRefs();

    //s.tableRemoveWhite(s.strings);
    s.sweep();

    s.nextGC = s.bytesAllocated * GCopts.GC_HEAP_GROW_FACTOR;
    if (GCopts.debug) {
        std.debug.print("-- gc end\n", .{});
        std.debug.print(
            "collected {d} bytes from {d} to {d} next at {d}\n",
            .{
                before - s.bytesAllocated,
                before,
                s.bytesAllocated,
                s.nextGC,
            },
        );
    }
}

pub fn freeObj(s: *GC, o: *lx.Obj) void {
    if (GCopts.debug) std.debug.print("free {*}: {s}\n", .{ o, @tagName(o.tp) });
    switch (o.tp) {
        .Upvalue => s.allocator().destroy(o.as(lx.Upvalue)),
        .NativeFn => s.allocator().destroy(o.as(lx.NativeFn)),
        .Closure => {
            const clos = o.as(lx.Closure);
            s.allocator().free(clos.upValues);
            s.allocator().destroy(clos);
        },
        .Func => {
            const func = o.as(lx.Func);
            func.chunk.deinit();
            s.allocator().destroy(func.chunk);
            s.allocator().destroy(func);
        },
        .String => {
            const str = o.as(lx.Obj.from(.String));
            s.allocator().free(str.chars);
            s.allocator().destroy(str);
        },
        .Class => {
            const class = o.as(lx.Class);
            s.allocator().destroy(class);
        },
        .Instance => {
            const inst = o.as(lx.Instance);
            inst.deinit(s);
            s.allocator().destroy(inst);
        },
    }
}

pub fn markRoots(s: *GC, vm: *lx.VM) void {
    const end = (@intFromPtr(vm.sp) - @intFromPtr(&vm.stack)) / @sizeOf(lx.Value);

    for (vm.stack[0..end]) |v| {
        s.markValue(v);
    }

    s.markTable(vm.globals);
    for (vm.frames[0..vm.frameCount]) |f| {
        s.markObj(f.clos);
    }

    var upvalue = vm.openUpValues;
    while (upvalue) |uv| {
        s.markObj(uv);
        upvalue = uv.next;
    }

    // todo compiler roots
    //s.markCompilerRoots();
}

pub fn markValue(gc: *GC, v: lx.Value) void {
    if (std.meta.activeTag(v) == .Obj) gc.markObj(v.Obj);
}

pub fn markObj(gc: *GC, oo: anytype) void {
    const o: *lx.Obj = switch (@TypeOf(oo)) {
        *lx.Obj => oo,
        inline else => &oo.obj,
    };
    if (o.isMarked) return;
    o.isMarked = true;

    if (GCopts.debug) {
        std.debug.print("mark {*} {{{any}}}\n", .{ o, oo });
    }

    gc.greyStack.append(o) catch @panic("OOM");
}

pub fn markTable(gc: *GC, t: anytype) void {
    var iter = t.valueIterator();
    while (iter.next()) |e| {
        //gc.markObj(e.key_ptr);
        gc.markValue(e.*);
    }
}

fn markCompilerRoots(gc: *GC, p: *lx.Parser) void {
    var compiler = p.current;
    while (compiler) |c| {
        gc.markObj(c.function);
        compiler = c.enclosing;
    }
}

fn traceRefs(s: *GC) void {
    while (s.greyStack.popOrNull()) |obj| {
        s.blackenObj(obj);
    }
}

fn blackenObj(s: *GC, o: *lx.Obj) void {
    switch (o.tp) {
        .Closure => {
            const clos = o.as(lx.Closure);
            s.markObj(clos.func);

            for (clos.upValues) |uv| {
                s.markObj(uv);
            }
        },
        .NativeFn, .String => {},
        .Upvalue => s.markValue(o.as(lx.Upvalue).closed),
        .Func => {
            const f = o.as(lx.Func);
            if (f.name) |str| s.markObj(str);
            for (f.chunk.vals.items) |v| {
                s.markValue(v);
            }
        },
        .Class => s.markObj(o.as(lx.Class).name),
        .Instance => {
            const inst = o.as(lx.Instance);
            s.markObj(inst.class);
            var iter = inst.fields.valueIterator();
            while (iter.next()) |field| {
                s.markValue(field.*);
            }
        },
    }

    if (GCopts.debug) {
        std.debug.print("{*} blacken {}\n", .{ o, lx.Value{ .Obj = o } });
    }
}

fn sweep(s: *GC) void {
    var prev: ?*lx.Obj = null;
    var obj = s.vm.objects;

    while (obj) |o| {
        if (o.isMarked) {
            o.isMarked = false;
            prev = o;
            obj = o.next;
        } else {
            obj = o.next;
            if (prev) |p| {
                p.next = obj;
            } else {
                s.vm.objects = obj;
            }

            s.freeObj(o);
        }
    }
}

fn tableRemoveWhite(s: *GC) void {
    if (s.greyStack.items.len != 0) @panic("stack not empty");

    var iter = s.strings.keyIterator();
    while (iter.next()) |*str| {
        if (!str.obj.isMarked) {
            s.greyStack.append(&str.obj);
        }
    }

    while (s.greyStack.popOrNull()) |o| {
        s.strings.removeByPtr(o.as(lx.String));
    }
}
