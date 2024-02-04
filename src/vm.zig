const std = @import("std");
const Allocator = std.mem.Allocator;
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

const StackMax = 512;

pub const VM = struct {
    alloc: Allocator,
    chunk: *Chunk,
    ip: [*]u8 = undefined,
    sp: [*]Value = undefined,
    stack: [StackMax]Value = [_]Value{0} ** StackMax,

    const Self = @This();
    pub fn init(alloc: Allocator) Self {
        var s: Self = .{
            .alloc = alloc,
            .sp = undefined,
            .chunk = undefined,
        };

        s.sp = &s.stack;
        return s;
    }

    pub fn deinit(s: *Self) void {
        _ = s;
    }

    pub fn interpret(s: *Self, chunk: *Chunk) void {
        s.ip = chunk.code.ptr;
        s.chunk = chunk;
    }

    pub fn run(s: *Self) !void {
        outer: while (true) {
            const instr: OpCode = @enumFromInt(s.incp());
            switch (instr) {
                inline .ADD, .SUB, .MUL, .DIV => |op| s.binOp(op),
                .CONST => {
                    const con = s.readConst();
                    s.push(con);
                },
                .NEGATE => s.push(-s.pop()),
                .RETURN => {
                    std.debug.print("returned: {any}\n", .{s.pop()});
                    break :outer;
                },
                //else => unreachable,
            }
        }
    }

    fn binOp(s: *Self, op: OpCode) void {
        var b = s.pop();
        var a = s.pop();

        //std.math.add
        var res = switch (op) {
            .ADD => a + b,
            .SUB => a - b,
            .MUL => a * b,
            .DIV => a / b,
            else => unreachable,
        };
        s.push(res);
    }

    fn readConst(s: *Self) Value {
        return s.chunk.vals.items[s.incp()];
    }

    fn incp(s: *Self) u8 {
        const b = s.ip[0];
        s.ip += 1;
        return b;
    }

    pub fn resetStack(s: *Self) u8 {
        s.sp = &s.stack;
    }
    fn pop(s: *Self) Value {
        s.sp -= 1;
        return s.sp[0];
    }

    fn push(s: *Self, v: Value) void {
        // todo overflow
        s.sp[0] = v;
        s.sp += 1;
    }
};
