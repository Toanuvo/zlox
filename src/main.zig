const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("chunk.zig").OpCode;
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;

pub fn main() !void {
    const args = std.os.argv;
    if (args.len == 1) {
        try repl();
    } else if (args.len == 2) {
        try runFile(args[1]);
    } else {
        unreachable;
    }

    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});
}

pub fn runFile(fileName: []const u8) !void {
    _ = fileName;
}

pub fn repl() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    var alloc = gpa.allocator();
    _ = alloc;
    defer _ = gpa.deinit();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    _ = stdout;
    defer bw.flush();

    const stdin = std.io.getStdIn().reader();
    _ = stdin;

    while (true) {}
}

test "simple test" {
    var alloc = std.testing.allocator;
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var vm = VM.init(alloc);
    defer _ = &vm.deinit();

    var c = Chunk.init(alloc);
    defer c.deinit();

    var cst = try c.addConst(1.2);
    try c.writeChunk(OpCode.CONST, 5);
    try c.writeChunk(cst, 5);

    var a1 = try c.addConst(3.4);
    try c.writeChunk(OpCode.CONST, 5);
    try c.writeChunk(a1, 5);

    try c.writeChunk(OpCode.ADD, 5);

    var a2 = try c.addConst(5.6);
    try c.writeChunk(OpCode.CONST, 5);
    try c.writeChunk(a2, 5);

    try c.writeChunk(OpCode.DIV, 5);
    try c.writeChunk(OpCode.NEGATE, 5);

    try c.writeChunk(OpCode.RETURN, 5);
    try debug.dissChunk(&c, stdout, "test");
    vm.interpret(&c);
    try vm.run();
    try bw.flush(); // don't forget to flush!

}
