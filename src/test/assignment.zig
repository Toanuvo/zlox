const std = @import("std");
const util = @import("test.zig");

test "associativity" {
    std.log.info("START ASSOC\n", .{});
    const alloc = std.testing.allocator;

    var out = std.ArrayList(u8).init(alloc);
    defer out.deinit();

    const vm = try util.setup(alloc, out.writer().any());
    defer util.cleanup(alloc, vm);

    const src =
        \\var a = "a";
        \\var b = "b";
        \\var c = "c";
        // Assignment is right-associative.
        \\a = b = c;
        \\print a; 
        \\print b; 
        \\print c; 
    ;

    const expect =
        \\"c"
        \\"c"
        \\"c"
        \\
    ;

    try vm.interpret(src);
    try std.testing.expectEqualStrings(expect, out.items);
    std.log.debug("CLEARED TEST\n", .{});
}
