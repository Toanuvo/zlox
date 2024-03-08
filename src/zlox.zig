pub usingnamespace @import("scanner.zig");
pub usingnamespace @import("memory.zig");
pub usingnamespace @import("chunk.zig");
pub usingnamespace @import("compiler.zig");
pub usingnamespace @import("vm.zig");
pub usingnamespace @import("value.zig");
pub usingnamespace @import("object.zig");
pub usingnamespace @import("object.zig").Types;
pub const std = @import("std");
pub const std_options = .{
    .log_level = std.log.Level.err,
};

comptime {
    _ = @import("test/assignment.zig");
}

test "run tests" {
    @import("std").log.info("val: \n", .{});
    @import("std").testing.refAllDecls(@This());
}
