const std = @import("std");
const Allocator = std.mem.Allocator;
const lx = @import("zlox.zig");
const mem = std.mem;
const Endianess = @import("builtin").cpu.arch.endian();

const StackMax = 512;
const FRAME_MAX = 64;

pub const RuntimeErrors = error{
    IncorrectOperandTypes,
};

pub const VM = struct {
    alloc: Allocator,
    //chunk: *lx.Chunk,
    //curFrame.ip: [*]u8 = undefined,
    sp: [*]lx.Value = undefined,
    objects: ?*lx.Obj,
    strings: lx.StrMap,
    globals: std.StringHashMap(lx.Value),
    stack: [StackMax]lx.Value = [_]lx.Value{lx.Value.Nil} ** StackMax,
    frames: [FRAME_MAX]CallFrame = [_]CallFrame{undefined} ** FRAME_MAX,
    frameCount: usize = 0,
    curFrame: *CallFrame = undefined,

    const Self = @This();
    pub fn init(alloc: Allocator) !*Self {
        const s = try alloc.create(Self);
        s.* = .{
            .alloc = alloc,
            //s.chunk = undefined;
            .objects = null,
            .strings = lx.StrMap.init(alloc),
            .globals = std.StringHashMap(lx.Value).init(alloc),
        };

        s.resetStack();
        try s.defineNative("clock", &VM.clockNative);

        return s;
    }

    pub fn deinit(s: *Self) void {
        while (s.objects) |objs| {
            // todo fix string interining double free
            var o = objs.*;
            s.objects = objs.next;
            inline for (comptime std.enums.values(lx.Type)) |t| {
                if (o.tp == t) {
                    o.as(t.as_type()).destroy(s.alloc);
                }
            }
        }
        s.strings.deinit();
        s.globals.deinit();
    }

    pub fn touchObj(s: *Self, obj: *lx.Obj) void {
        obj.next = s.objects;
        s.objects = obj;
    }

    pub fn interpret(s: *Self, src: [:0]const u8) !void {
        var cp = try lx.Parser.init(s.alloc, s, src);
        var cpp = &cp;
        const func = try cpp.compile();

        //const obj: *lx.Obj = &func.obj;
        //std.debug.print("func: {any}\n", .{func.chunk});
        //std.debug.print("obj: {any}\n", .{func.obj.tp});
        //std.debug.print("obj: {any}\n", .{func.obj.next});
        //std.debug.print("obj: {X}\n", .{@intFromPtr(s.sp)});

        var o = s.objects;
        while (o) |obj| {
            //std.debug.print("val: {any}\n", .{obj});
            o = obj.next;
        }

        s.push(*lx.Obj, &func.obj);
        _ = s.call(func, 0);
        try s.run();
    }

    pub fn run(s: *Self) !void {
        s.curFrame = &s.frames[s.frameCount - 1];
        outer: while (true) {
            const instr: OpCode = @enumFromInt(s.incp());

            //std.debug.print("instr: {any}\n", .{instr});
            //std.debug.print("stack: {any}\n", .{s.sp[0]});
            switch (instr) {
                .PRINT => {
                    std.debug.print("{any}\n", .{s.pop()}); // todo stdio
                },
                .ADD => {
                    const a = s.peek(0);
                    const b = s.peek(1);
                    try switch (getMatchInt(a, b)) {
                        mswi(.Obj, .Obj) => {
                            if (a.Obj.is(lx.String) and b.Obj.is(lx.String)) {
                                try s.concat();
                            } else {
                                try s.runtimeError("ops must be string");
                            }
                        },
                        mswi(.Num, .Num) => s.binOp(.ADD, .Num),
                        else => s.runtimeError("ops must be string or int"),
                    };
                },
                inline .SUB, .MUL, .DIV => |op| try s.binOp(op, .Num),
                inline .GTR, .LESS => |op| try s.binOp(op, .Bool),
                .CONST => {
                    const con = s.readConst();
                    //std.debug.print("const: {any}\n", .{con});
                    s.push(lx.Value, con);
                },
                .NEGATE => switch (s.peek(0)) {
                    .Num => s.push(lx.Value, .{ .Num = -s.pop().Num }),
                    else => {
                        try s.runtimeError("operand must be a number");
                        return error.IncorrectOperandTypes;
                    },
                },
                .RETURN => {
                    const res = s.pop();
                    s.frameCount -= 1;
                    if (s.frameCount == 0) {
                        _ = s.pop();
                        if (@intFromPtr(s.sp) != @intFromPtr(&s.stack)) {
                            @panic("stack not empty");
                        }
                        break :outer;
                    }

                    s.sp = s.curFrame.slots;
                    s.push(lx.Value, res);
                    s.curFrame = &s.frames[s.frameCount - 1];
                },
                .TRUE => s.push(bool, true),
                .FALSE => s.push(bool, false),
                .NOT => s.push(bool, s.pop().isFalsey()),
                .NIL => s.push(lx.ValueTag, .Nil),
                .POP => _ = s.pop(),
                .DEF_GLOB => {
                    const str = s.readConst().Obj.as(lx.String);
                    try s.globals.put(str.chars, s.peek(0));
                    _ = s.pop();
                },
                .SET_GLOB => {
                    const name = s.readConst().Obj.as(lx.String);
                    if (s.globals.getPtr(name.chars)) |v| {
                        v.* = s.peek(0);
                    } else {
                        try s.runtimeError("undefined global variable");
                        std.debug.print(" {s}\n", .{name.chars});
                    }
                },
                .GET_GLOB => {
                    const str = s.readConst().Obj.as(lx.String);
                    if (s.globals.get(str.chars)) |v| {
                        s.push(lx.Value, v);
                    } else {
                        try s.runtimeError("undefind global variable");
                        std.debug.print(" {s}\n", .{str.chars});
                    }
                },
                .SET_LOCAL => {
                    const slot = s.incp();
                    s.curFrame.slots[slot] = s.peek(0);
                },
                .GET_LOCAL => {
                    const slot: usize = s.incp();
                    s.push(lx.Value, s.curFrame.slots[slot]);
                },
                .JMP_IF_FALSE => {
                    const offset = s.read(u16);
                    //std.debug.print("read {any}\n", .{offset});
                    if (s.peek(0).isFalsey()) s.curFrame.ip += offset;
                },
                .JMP => {
                    const i = s.read(u16);
                    //std.debug.print("read {any}\n", .{i});
                    s.curFrame.ip += i;
                },
                .LOOP => {
                    s.curFrame.ip -= s.read(u16);
                },
                .CALL => {
                    const argCount = s.incp();
                    //std.debug.print("args: {any}\n", .{argCount});
                    if (!s.callValue(s.peek(argCount), argCount)) {
                        @panic("runtime error");
                    }
                    s.curFrame = &s.frames[s.frameCount - 1];
                },
                .EQL => {
                    const b = s.pop();
                    const a = s.pop();
                    s.push(bool, a.equals(b));
                },
                //else => unreachable,
            }
        }
    }

    fn read(s: *Self, comptime T: type) T {
        if (T == u8) {
            return s.incp();
        }
        const v = mem.readInt(T, s.curFrame.ip[0..@sizeOf(T)], Endianess);
        s.curFrame.ip += @sizeOf(T);
        return v;
    }

    fn callValue(s: *Self, callee: lx.Value, argCount: u8) bool {
        switch (callee) {
            .Obj => |o| {
                switch (o.tp) {
                    .Func => return s.call(o.as(lx.Func), argCount),
                    .NativeFn => {
                        const native = o.as(lx.NativeFn).native;
                        const res = native(argCount, (s.sp - argCount)[0..argCount]);
                        s.sp -= argCount + 1;
                        s.push(lx.Value, res);
                        return true;
                    },
                    else => @panic("obj not callable"),
                }
            },
            inline else => |v, t| {
                std.debug.print("{any}: {any}\n", .{ t, v });
                @panic("callee not an object");
            },
        }
        return false;
    }

    fn call(s: *Self, func: *lx.Func, argCount: u8) bool {
        if (argCount != func.arity) {
            @panic("unexpected arg count");
        }
        if (s.frameCount == FRAME_MAX) @panic("stack overflow");
        s.curFrame = &s.frames[s.frameCount];
        s.frameCount += 1;
        s.curFrame.* = .{
            .func = func,
            .ip = func.chunk.code.ptr,
            .slots = s.sp - argCount - 1,
        };
        return true;
    }

    fn defineNative(s: *Self, name: []const u8, func: lx.NativeFn.NativeFnT) !void {
        s.push(*lx.Obj, &(try lx.String.allocString(s, s.alloc, name)).obj);
        s.push(*lx.Obj, &(try lx.NativeFn.init(s.alloc, s, func)).obj);
        try s.globals.put(s.stack[0].Obj.as(lx.String).*.chars, s.stack[1]);
        _ = s.pop();
        _ = s.pop();
    }

    pub fn clockNative(_: u8, _: []lx.Value) lx.Value {
        return .{ .Num = @floatFromInt(std.time.microTimestamp() * std.time.ms_per_s) };
    }

    fn concat(s: *Self) !void {
        const b = s.pop().Obj.as(lx.String).chars;
        const a = s.pop().Obj.as(lx.String).chars;

        const new = try std.mem.concatWithSentinel(s.alloc, u8, &[_][]const u8{ a, b }, 0);
        const str = try lx.String.takeString(s.alloc, new);
        s.push(*lx.Obj, &str.obj);
    }

    fn peek(s: *Self, dist: usize) lx.Value {
        return ((s.sp - 1) - dist)[0];
    }

    fn binOp(s: *Self, op: lx.OpCode, comptime resType: lx.ValueTag) !void {
        if (!s.peek(0).is(.Num) or !s.peek(1).is(.Num)) {
            std.debug.print("got type: {any}\n", .{s.peek(0)});
            try s.runtimeError("operands must be numbers");
            // todo print stack trace
            return error.IncorrectOperandTypes;
        }

        const b = s.pop().Num;
        const a = s.pop().Num;

        //std.math.add
        const res: lx.Value = switch (resType) {
            .Bool => .{ .Bool = switch (op) {
                .LESS => a < b,
                .GTR => a > b,
                else => unreachable,
            } },
            .Num => .{ .Num = switch (op) {
                .ADD => a + b,
                .SUB => a - b,
                .MUL => a * b,
                .DIV => a / b,
                else => unreachable,
            } },
            else => unreachable,
        };
        s.push(lx.Value, res);
    }

    fn readConst(s: *Self) lx.Value {
        //std.debug.print("{*}\n", .{s.curFrame.func.chunk});
        return s.curFrame.func.chunk.vals.items[s.incp()];
    }

    fn incp(s: *Self) u8 {
        const b = s.curFrame.ip[0];
        s.curFrame.ip += 1;
        return b;
    }

    pub fn resetStack(s: *Self) void {
        s.sp = &s.stack;
        s.frameCount = 0;
    }

    fn pop(s: *Self) lx.Value {
        s.sp -= 1;
        std.debug.assert(@intFromPtr(s.sp) >= @intFromPtr(&s.stack)); // stack underflow
        return s.sp[0];
    }

    fn push(s: *Self, comptime T: type, v: T) void {
        //std.debug.print("val: {*}\n", .{&v});
        //std.debug.print("val: {any}\n", .{v});
        s.sp[0] = switch (T) {
            lx.Value => v,
            f64 => .{ .Num = v },
            bool => .{ .Bool = v },
            *lx.Obj => .{ .Obj = v },
            lx.ValueTag => lx.Value{ .Nil = {} },
            //@TypeOf(.enum_literal), lx.ValueTag => @as(lx.Value, v), // should be nil
            else => |t| {
                std.debug.print("got type: {any} for value: {any}\n", .{ t, v });
                @panic("tried to push unexpected value type onto stack");
            },
        };

        // todo overflow
        //std.debug.print("got type: {s}\n", .{@typeName(@TypeOf(v))});
        //std.debug.print("setting: {any}\n", .{std.meta.activeTag(s.stack[S.spp])});
        s.sp += 1;
    }

    fn runtimeError(s: *Self, msg: [:0]const u8) !void {
        var stderr = std.io.getStdErr().writer();
        try stderr.print("err: {s}\n", .{msg});

        const instr = @intFromPtr(s.curFrame.ip) - @intFromPtr(s.curFrame.func.chunk.code.ptr) - 1;

        const line = s.curFrame.func.chunk.lines[instr];
        try stderr.print("[{d}] in scrcurFrame.ipt\n", .{line});
        s.resetStack();
    }

    fn getMatchInt(a: lx.ValueTag, b: lx.ValueTag) u8 {
        const t: u8 = @as(u8, @intFromEnum(a)) << 4;
        return t | @intFromEnum(b);
    }

    // make switch int
    fn mswi(comptime a: lx.ValueTag, comptime b: lx.ValueTag) u8 {
        const t: u8 = @as(u8, @intFromEnum(a)) << 4;
        return t | @intFromEnum(b);
    }
};

pub const CallFrame = struct {
    func: *lx.Func,
    ip: [*]u8,
    slots: [*]lx.Value,
};

pub const OpCode = enum(u8) {
    RETURN,
    DEF_GLOB,
    GET_GLOB,
    SET_GLOB,
    GET_LOCAL,
    SET_LOCAL,
    JMP_IF_FALSE,
    JMP,
    LOOP,
    CONST,
    NEGATE,
    PRINT,
    CALL,
    POP,
    ADD,
    SUB,
    MUL,
    DIV,
    NIL,
    TRUE,
    FALSE,
    NOT,
    EQL,
    GTR,
    LESS,
};

test VM {
    std.debug.print("\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = false }){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();
    var vm = try VM.init(alloc);
    //defer vm.deinit();

    //const tst = "!(5 - 4 > 3 * 2 == !nil)";
    //const tst = "\"hello\"==\"hello\"";
    //const tst = "print 1+1;";
    const tst =
        //"var beverage = \"cafe au lait\";\n" ++
        //"var breakfast = \"beignets\";\n" ++
        //"if(true){\n" ++
        //"for (var i = 0; i < 5; i = i + 1) {\n" ++
        //"breakfast = \"beignets with \"+ beverage;\n" ++
        //"print breakfast;}";
        "fun fib(n) {\n" ++
        "if (n < 2) return n;\n" ++
        "return fib(n - 2) + fib(n - 1); }\n" ++
        "var start = clock();\n" ++
        "print fib(10);\n" ++
        "print clock() - start;\n";

    //try vm.interpret("!(5 - 4 > 3 * 2 == !nil)");
    //errdefer c.deinit();

    //vm.curFrame.ip = c.code.ptr;
    //vm.chunk = &c;
    try vm.interpret(tst);
}
