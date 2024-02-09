const std = @import("std");
const Allocator = std.mem.Allocator;
const lx = @import("zlox.zig");

const StackMax = 512;

pub const RuntimeErrors = error{
    IncorrectOperandTypes,
};

pub const VM = struct {
    alloc: Allocator,
    chunk: *lx.Chunk,
    ip: [*]u8 = undefined,
    sp: [*]lx.Value = undefined,
    stack: [StackMax]lx.Value = [_]lx.Value{lx.Value.Nil} ** StackMax,
    objects: ?*lx.Obj,
    strings: lx.StrMap,

    const Self = @This();
    pub fn init(alloc: Allocator) Self {
        var s: Self = .{
            .alloc = alloc,
            .sp = undefined,
            .chunk = undefined,
            .objects = null,
            .strings = lx.StrMap.init(alloc),
        };

        s.sp = &s.stack;
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
    }

    pub fn interpret(s: *Self, src: [:0]const u8) !void {
        _ = src;
        _ = s;
        // todo implement
    }

    pub fn run(s: *Self) !void {
        outer: while (true) {
            const instr: lx.OpCode = @enumFromInt(s.incp());

            //std.debug.print("instr: {any}\n", .{instr});
            switch (instr) {
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
                    s.push(con);
                },
                .NEGATE => switch (s.peek(0)) {
                    .Num => s.push(.{ .Num = -s.pop().Num }),
                    else => {
                        try s.runtimeError("operand must be a number");
                        return error.IncorrectOperandTypes;
                    },
                },
                .RETURN => {
                    std.debug.print("returned: {any}\n", .{s.pop()});
                    break :outer;
                },
                .TRUE => s.push(true),
                .FALSE => s.push(false),
                .NOT => s.push(s.pop().isFalsey()),
                .NIL => s.push(.Nil),
                .EQL => {
                    const b = s.pop();
                    const a = s.pop();
                    s.push(a.equals(b));
                },
                //else => unreachable,
            }
        }
    }

    fn concat(s: *Self) !void {
        const b = s.pop().Obj.as(lx.String).chars;
        const a = s.pop().Obj.as(lx.String).chars;

        const new = try std.mem.concatWithSentinel(s.alloc, u8, &[_][]const u8{ a, b }, 0);
        const str = try lx.String.takeString(s.alloc, new);
        s.push(str.obj);
    }

    fn peek(s: *Self, dist: usize) lx.Value {
        return ((s.sp - 1) - dist)[0];
    }

    fn binOp(s: *Self, op: lx.OpCode, comptime resType: lx.ValueTag) !void {
        if (!s.peek(0).is(.Num) or !s.peek(1).is(.Num)) {
            try s.runtimeError("operands must be numbers");
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
        s.push(res);
    }

    fn readConst(s: *Self) lx.Value {
        return s.chunk.vals.items[s.incp()];
    }

    fn incp(s: *Self) u8 {
        const b = s.ip[0];
        s.ip += 1;
        return b;
    }

    pub fn resetStack(s: *Self) void {
        s.sp = &s.stack;
    }

    fn pop(s: *Self) lx.Value {
        s.sp -= 1;
        return s.sp[0];
    }

    fn push(s: *Self, v: anytype) void {
        s.sp[0] = switch (@TypeOf(v)) {
            lx.Value => v,
            f64 => .{ .Num = v },
            bool => .{ .Bool = v },
            *lx.Obj => .{ .Obj = v },
            @TypeOf(.enum_literal), lx.ValueTag => @as(lx.Value, v), // should be nil
            else => |t| {
                std.debug.print("got type: {any} for value: {any}\n", .{ t, v });
                @panic("tried to push unexpected value type onto stack");
            },
        };
        // todo overflow
        s.sp += 1;
    }

    fn runtimeError(s: *Self, msg: [:0]const u8) !void {
        var stderr = std.io.getStdErr().writer();
        try stderr.print("err: {s}\n", .{msg});

        const instr = @intFromPtr(s.ip) - @intFromPtr(s.chunk.code.ptr) - 1;

        const line = s.chunk.lines[instr];
        try stderr.print("[{d}] in script\n", .{line});
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

test VM {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = false }){};
    const alloc = gpa.allocator();
    defer _ = gpa.deinit();
    var vm = VM.init(alloc);
    defer vm.deinit();

    //const tst = "!(5 - 4 > 3 * 2 == !nil)";
    const tst = "\"hello\"==\"hello\"";

    //try vm.interpret("!(5 - 4 > 3 * 2 == !nil)");
    var c = lx.Chunk.init(alloc);
    var cp = lx.Compiler.init(alloc, &vm, tst, &c);
    var cpp = &cp;
    _ = try cpp.compile();
    errdefer c.deinit();

    vm.ip = c.code.ptr;
    vm.chunk = &c;
    try vm.run();
}
