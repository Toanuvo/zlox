const std = @import("std");
const Allocator = std.mem.Allocator;
const lx = @import("zlox.zig");

const StackMax = 512;

pub const VM = struct {
    alloc: Allocator,
    chunk: *lx.Chunk,
    ip: [*]u8 = undefined,
    sp: [*]lx.Value = undefined,
    stack: [StackMax]lx.Value = [_]lx.Value{0} ** StackMax,

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

    pub fn interpret(s: *Self, src: [:0]const u8) !void {
        var c = lx.Chunk.init(s.alloc);
        var cp = lx.Compiler.init(s.alloc, src, &c);
        var cpp = &cp;
        _ = try cpp.compile();
        errdefer c.deinit();

        s.ip = c.code.ptr;
        s.chunk = &c;
    }

    pub fn run(s: *Self) !void {
        outer: while (true) {
            const instr: lx.OpCode = @enumFromInt(s.incp());
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

    fn binOp(s: *Self, op: lx.OpCode) void {
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

    fn readConst(s: *Self) lx.Value {
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
    fn pop(s: *Self) lx.Value {
        s.sp -= 1;
        return s.sp[0];
    }

    fn push(s: *Self, v: lx.Value) void {
        // todo overflow
        s.sp[0] = v;
        s.sp += 1;
    }
};

test VM {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = false }){};
    var alloc = gpa.allocator();
    defer _ = gpa.deinit();
    var vm = VM.init(alloc);
    defer vm.deinit();

    try vm.interpret("(-1 + 2) * 3 - -4");
    try vm.run();
}
