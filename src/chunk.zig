const std = @import("std");
const Allocator = std.mem.Allocator;
const mem = @import("memory.zig");
const Value = @import("value.zig").Value;
const debug = @import("debug.zig");

const ValueArr = std.ArrayList(Value);

pub const Chunk = struct {
    code: []u8 = std.mem.zeroes([]u8),
    lines: []u64 = std.mem.zeroes([]u64),
    cap: u64 = 0,
    alloc: Allocator,
    vals: ValueArr,

    const Self = @This();
    pub fn init(alloc: Allocator) Self {
        return .{
            .alloc = alloc,
            .vals = ValueArr.init(alloc),
        };
    }

    pub fn writeChunk(s: *Self, v: anytype, line: u64) !void {
        const byte: u8 = switch (@TypeOf(v)) {
            OpCode => @intFromEnum(v),
            usize => @intCast(v),
            else => unreachable,
        };
        if (s.code.len < s.cap + 1) {
            const newCap = if (s.cap < 8) 8 else s.cap * 2;
            s.code = try mem.realloc(s.alloc, s.code, newCap);
            s.lines = try mem.realloc(s.alloc, s.lines, newCap);
        }

        s.code[s.cap] = byte;
        s.lines[s.cap] = line;
        s.cap += 1;
    }

    pub fn addConst(s: *Self, v: Value) !usize {
        try s.vals.append(v);
        return s.vals.items.len - 1;
    }

    pub fn deinit(s: *Self) void {
        s.alloc.free(s.code);
        s.alloc.free(s.lines);
        s.vals.deinit();
        // do i need to zero fields?
    }
};

pub const OpCode = enum(u8) {
    RETURN,
    CONST,
    NEGATE,
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

test Chunk {
    const tup = .{ .RETURN, .ADD };
    try tst(&tup);
}

fn tst(comptime ops: []const OpCode) !void {
    for (ops) |value| {
        std.debug.print("returned: {any}\n", .{value});
    }
}
