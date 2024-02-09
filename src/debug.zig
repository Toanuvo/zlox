const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;

pub fn dissChunk(c: *const Chunk, stream: anytype, name: []const u8) !void {
    try stream.print("== {s} ==\n", .{name});
    var offset: usize = 0;
    while (offset < c.cap) {
        offset = try dissInstr(c, stream, offset);
    }
}
fn dissInstr(s: *const Chunk, stream: anytype, offset: usize) !usize {
    try stream.print("{d:0>4} ", .{offset});
    if (offset > 0 and s.lines[offset] == s.lines[offset - 1]) {
        try stream.print("   | ", .{});
    } else {
        try stream.print("{d:0>4} ", .{s.lines[offset]});
    }
    const instr = s.code[offset];
    if (instr < 256) {
        const op: OpCode = @enumFromInt(instr);
        return switch (op) {
            .RETURN,
            .ADD,
            .SUB,
            .MUL,
            .DIV,
            .NEGATE,
            => simpleInstr(s, @tagName(op), stream, offset),
            .CONST => constInstr(s, @tagName(op), stream, offset),
        };
    }
}

fn constInstr(s: *const Chunk, name: []const u8, stream: anytype, offset: usize) !usize {
    const idx = s.code[offset + 1];
    try stream.print("{s: <16} {d:0>4} '{e}'\n", .{ name, idx, s.vals.items[idx] });
    return offset + 2;
}

fn simpleInstr(s: *const Chunk, name: []const u8, stream: anytype, offset: usize) !usize {
    _ = s;
    try stream.print("{s}\n", .{name});
    return offset + 1;
}
