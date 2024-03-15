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
    gc: *lx.GC,
    //chunk: *lx.Chunk,
    //curFrame.ip: [*]u8 = undefined,
    sp: [*]lx.Value = undefined,
    objects: ?*lx.Obj,
    globals: lx.Table(lx.Value),
    stack: [StackMax]lx.Value = [_]lx.Value{lx.Value.Nil} ** StackMax,
    frames: [FRAME_MAX]CallFrame = [_]CallFrame{undefined} ** FRAME_MAX,
    frameCount: usize = 0,
    curFrame: *CallFrame = undefined,
    openUpValues: ?*lx.Upvalue = null,
    writer: std.io.AnyWriter,
    initString: ?*lx.String = null,

    const Self = @This();
    pub fn init(s: *Self, gc: *lx.GC, writer: std.io.AnyWriter) !void {
        const alloc = gc.allocator();

        s.* = .{
            .writer = writer,
            .gc = gc,
            .objects = null,
            .globals = lx.Table(lx.Value).init(alloc),
        };

        // create string after vm struct is created to avoid gc on partially
        // initialized vm struct
        gc.vm = s;
        s.initString = try gc.create(lx.String, .{
            .str = "init",
            .canTake = false,
        });

        s.resetStack();
        //try s.defineNative("clock", &VM.clockNative);
    }

    pub fn deinit(s: *Self) void {
        var object = s.objects;
        while (object) |obj| {
            object = obj.next;
            s.gc.freeObj(obj);
        }

        s.globals.deinit();
    }

    pub fn interpret(s: *Self, src: [:0]const u8) !void {
        const func = try lx.Parser.compile(s.gc, src);

        s.push(*lx.Obj, &func.obj);
        _ = s.pop();
        const clos = try s.gc.create(lx.Closure, func);
        s.push(*lx.Obj, &clos.obj);
        _ = s.call(clos, 0);
        try s.run();
    }

    pub fn run(s: *Self) !void {
        s.curFrame = &s.frames[s.frameCount - 1];
        outer: while (true) {
            const instr: OpCode = @enumFromInt(s.incp());

            //std.log.debug("instr: {any}\n", .{instr});
            //std.log.debug("stack: {any}\n", .{s.sp[0]});
            switch (instr) {
                .PRINT => {
                    try s.writer.print("{any}\n", .{s.pop()});
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
                    //std.log.debug("const: {any}\n", .{con});
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
                    s.closeUpValues(&s.curFrame.slots[0]);
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
                    try s.globals.put(str, s.peek(0));
                    _ = s.pop();
                },
                .SET_GLOB => {
                    const name = s.readConst().Obj.as(lx.String);
                    if (s.globals.getPtr(name)) |v| {
                        v.* = s.peek(0);
                    } else {
                        try s.runtimeError("undefined global variable");
                        std.log.debug(" {s}\n", .{name.chars});
                    }
                },
                .GET_GLOB => {
                    const str = s.readConst().Obj.as(lx.String);
                    if (s.globals.get(str)) |v| {
                        s.push(lx.Value, v);
                    } else {
                        try s.runtimeError("undefind global variable");
                        std.log.debug(" {s}\n", .{str.chars});
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
                .SET_PROP => {
                    if (!s.peek(1).Obj.is(lx.Instance))
                        try s.runtimeError("only instances have properties");
                    const inst = s.peek(1).Obj.as(lx.Instance);
                    const key = s.readConst().Obj.as(lx.String);
                    try inst.fields.put(key, s.peek(0));

                    const v = s.pop();
                    _ = s.pop();
                    s.push(lx.Value, v);
                },
                .GET_PROP => {
                    if (!s.peek(0).Obj.is(lx.Instance))
                        try s.runtimeError("only instances have properties");
                    const inst = s.peek(0).Obj.as(lx.Instance);
                    const name = s.readConst().Obj.as(lx.String);

                    if (inst.fields.get(name)) |v| {
                        _ = s.pop(); // instance
                        s.push(lx.Value, v);
                    }

                    if (!(try s.bindMethod(inst.class, name))) {
                        try s.runtimeError("undefined property");
                    }
                },
                .JMP_IF_FALSE => {
                    const offset = s.read(u16);
                    //std.log.debug("read {any}\n", .{offset});
                    if (s.peek(0).isFalsey()) s.curFrame.ip += offset;
                },
                .JMP => {
                    const i = s.read(u16);
                    //std.log.debug("read {any}\n", .{i});
                    s.curFrame.ip += i;
                },
                .LOOP => {
                    s.curFrame.ip -= s.read(u16);
                },
                .CLOSURE => {
                    const func = s.readConst().Obj.as(lx.Func);
                    const clos = try s.gc.create(lx.Closure, func);
                    s.push(*lx.Obj, &clos.obj);
                    for (0..clos.upValues.len) |i| {
                        const isLocal = s.incp() == 1;
                        const idx = s.incp();
                        clos.upValues[i] = if (isLocal)
                            try s.captureUpValue(@ptrCast(s.curFrame.slots + idx))
                        else
                            s.curFrame.clos.upValues[idx];
                    }
                },
                .GET_UPVAL => {
                    const slot = s.incp();
                    s.push(lx.Value, s.curFrame.clos.upValues[slot].location.*);
                },
                .SET_UPVAL => {
                    const slot = s.incp();
                    s.curFrame.clos.upValues[slot].location.* = s.peek(0);
                },
                .CLOSE_UP_VAL => {
                    s.closeUpValues(&(s.sp - 1)[0]);
                    _ = s.pop();
                },
                .CALL => {
                    const argCount = s.incp();
                    //std.log.debug("args: {any}\n", .{argCount});
                    if (!(try s.callValue(s.peek(argCount), argCount))) {
                        @panic("runtime error");
                    }
                    s.curFrame = &s.frames[s.frameCount - 1];
                },
                .EQL => {
                    const b = s.pop();
                    const a = s.pop();
                    s.push(bool, a.equals(b));
                },
                .CLASS => {
                    const class = try s.gc.create(
                        lx.Class,
                        s.readConst().Obj.as(lx.String),
                    );
                    s.push(lx.Value, .{ .Obj = &class.obj });
                },
                .METHOD => try s.defineMethod(s.readConst().Obj.as(lx.String)),
                .INVOKE => {
                    const m = s.readConst().Obj.as(lx.String);
                    const argCount = s.incp();
                    if (!(try s.invoke(m, argCount))) {
                        try s.runtimeError("invoke");
                    }

                    s.curFrame = &s.frames[s.frameCount - 1];
                },
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

    fn invoke(s: *Self, name: *lx.String, argCount: u8) !bool {
        const reciever = s.peek(argCount);
        if (!reciever.Obj.is(lx.Instance)) {
            try s.runtimeError("only instances have methods");
            return false;
        }
        const inst = reciever.Obj.as(lx.Instance);

        if (inst.fields.get(name)) |v| {
            (s.sp - argCount - 1)[0] = v;
            return try s.callValue(v, argCount);
        } else {
            return try s.invokeFromClass(inst.class, name, argCount);
        }
    }

    fn invokeFromClass(s: *Self, class: *lx.Class, name: *lx.String, argCount: u8) !bool {
        if (class.methods.get(name)) |m| {
            return s.call(m.Obj.as(lx.Closure), argCount);
        } else {
            try s.runtimeError("undefind property");
            return false;
        }
    }

    fn bindMethod(s: *Self, class: *lx.Class, name: *lx.String) !bool {
        const method = class.methods.get(name);

        if (method) |m| {
            const bound = try s.gc.create(lx.BoundMethod, .{ s.peek(0), m.Obj.as(lx.Closure) });
            _ = s.pop();
            s.push(lx.Value, .{ .Obj = &bound.obj });
            return true;
        } else {
            try s.runtimeError("undefind property");
            return false;
        }
    }

    fn defineMethod(s: *Self, name: *lx.String) !void {
        const method = s.peek(0);
        const class = s.peek(1).Obj.as(lx.Class);
        try class.methods.put(name, method);
        _ = s.pop();
    }

    fn captureUpValue(s: *Self, local: *lx.Value) !*lx.Upvalue {
        var prev: ?*lx.Upvalue = null;
        var upValue = s.openUpValues;
        while (upValue) |uv| {
            if (@intFromPtr(uv.location) > @intFromPtr(local)) break;
            prev = uv;
            upValue = uv.next;
        }

        if (upValue) |uv|
            if (@intFromPtr(uv.location) == @intFromPtr(local)) {
                return uv;
            };

        const created = try s.gc.create(lx.Upvalue, local);
        created.next = upValue;
        if (prev) |p| {
            p.next = created;
        } else {
            s.openUpValues = created;
        }
        return created;
    }

    fn closeUpValues(s: *Self, last: *lx.Value) void {
        while (s.openUpValues) |uv| {
            if (@intFromPtr(last) <= @intFromPtr(uv.location)) break;
            uv.closed = uv.location.*;
            uv.location = &uv.closed;
            s.openUpValues = uv.next;
        }
    }

    fn callValue(s: *Self, callee: lx.Value, argCount: u8) !bool {
        switch (callee) {
            .Obj => |o| {
                switch (o.tp) {
                    .BoundMethod => {
                        const bm = o.as(lx.BoundMethod);
                        (s.sp - argCount - 1)[0] = bm.reciever;
                        return s.call(bm.method, argCount);
                    },
                    .Closure => return s.call(o.as(lx.Closure), argCount),
                    .Class => {
                        const class = o.as(lx.Class);
                        (s.sp - argCount - 1)[0] = .{ .Obj = &(try s.gc.create(lx.Instance, class)).obj };
                        if (class.methods.get(s.initString.?)) |inzlr| {
                            return s.call(inzlr.Obj.as(lx.Closure), argCount);
                        } else if (argCount != 0) {
                            try s.runtimeError("expected 0 arguments");
                            return false;
                        }
                        return true;
                    },
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
                std.log.debug("{any}: {any}\n", .{ t, v });
                @panic("callee not an object");
            },
        }
        return false;
    }

    fn call(s: *Self, clos: *lx.Closure, argCount: u8) bool {
        const func = clos.func;
        if (argCount != func.arity) {
            @panic("unexpected arg count");
        }

        //std.log.debug("== {s} ==\n", .{if (func.name) |n| n.chars else "SCRIPT"});
        //std.log.debug("{func}\n", .{clos.func.chunk});

        if (s.frameCount == FRAME_MAX) @panic("stack overflow");
        s.curFrame = &s.frames[s.frameCount];
        s.frameCount += 1;
        s.curFrame.* = .{
            .clos = clos,
            .ip = func.chunk.code.ptr,
            .slots = s.sp - argCount - 1,
        };
        return true;
    }

    fn defineNative(s: *Self, name: []const u8, func: lx.NativeFn.NativeFnT) !void {
        s.push(*lx.Obj, &(try s.gc.create(lx.String, .{ .str = name })).obj);
        s.push(*lx.Obj, &(try s.gc.create(lx.NativeFn, func)).obj);
        try s.globals.put(s.stack[0].Obj.as(lx.String), s.stack[1]);
        _ = s.pop();
        _ = s.pop();
    }

    pub fn clockNative(_: u8, _: []lx.Value) lx.Value {
        return .{ .Num = @floatFromInt(std.time.microTimestamp() * std.time.ms_per_s) };
    }

    fn concat(s: *Self) !void {
        const b = s.peek(0).Obj.as(lx.String).chars;
        const a = s.peek(1).Obj.as(lx.String).chars;

        const new = try std.mem.concatWithSentinel(s.gc.allocator(), u8, &[_][]const u8{ a, b }, 0);
        const str = try s.gc.create(lx.String, .{ .str = new, .canTake = true });
        _ = s.pop();
        _ = s.pop();
        s.push(*lx.Obj, &str.obj);
    }

    fn peek(s: *Self, dist: usize) lx.Value {
        return ((s.sp - 1) - dist)[0];
    }

    fn binOp(s: *Self, op: lx.OpCode, comptime resType: lx.ValueTag) !void {
        if (!s.peek(0).is(.Num) or !s.peek(1).is(.Num)) {
            std.log.debug("got type: {any}\n", .{s.peek(0)});
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
        //std.log.debug("{*}\n", .{s.curFrame.clos.func.chunk});
        return s.curFrame.clos.func.chunk.vals.items[s.incp()];
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
        //std.log.debug("val: {*}\n", .{&v});
        //std.log.debug("val: {any}\n", .{v});
        s.sp[0] = switch (T) {
            lx.Value => v,
            f64 => .{ .Num = v },
            bool => .{ .Bool = v },
            *lx.Obj => .{ .Obj = v },
            lx.ValueTag => lx.Value{ .Nil = {} },
            //@TypeOf(.enum_literal), lx.ValueTag => @as(lx.Value, v), // should be nil
            else => |t| {
                std.log.debug("got type: {any} for value: {any}\n", .{ t, v });
                @panic("tried to push unexpected value type onto stack");
            },
        };

        // todo overflow
        //std.log.debug("got type: {s}\n", .{@typeName(@TypeOf(v))});
        //std.log.debug("setting: {any}\n", .{std.meta.activeTag(s.stack[S.spp])});
        s.sp += 1;
    }

    fn runtimeError(s: *Self, msg: [:0]const u8) !void {
        var stderr = std.io.getStdErr().writer();
        try stderr.print("err: {s}\n", .{msg});

        const instr = @intFromPtr(s.curFrame.ip) - @intFromPtr(s.curFrame.clos.func.chunk.code.ptr) - 1;

        const line = s.curFrame.clos.func.chunk.lines[instr];
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
    clos: *lx.Closure,
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
    GET_UPVAL,
    SET_UPVAL,
    GET_PROP,
    SET_PROP,
    JMP_IF_FALSE,
    JMP,
    CLOSURE,
    CLOSE_UP_VAL,
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
    CLASS,
    METHOD,
    INVOKE,
};
