const std = @import("std");
const tag = std.meta.activeTag;
const lx = @import("zlox.zig");

pub const ValueTag = enum { Bool, Num, Obj, Nil };

pub const Value = union(ValueTag) {
    Bool: bool,
    Num: f64,
    Obj: *lx.Obj,
    Nil,

    const Self = @This();
    pub fn equals(a: Self, b: Self) bool {
        if (tag(a) != tag(b)) return false;
        const act = switch (a) {
            .Nil => true,
            .Bool => a.Bool == b.Bool,
            .Num => a.Num == b.Num,
            .Obj => |o| switch (o.tp) {
                .String => a.Obj.as(lx.String).hv == b.Obj.as(lx.String).hv,
            },
            //else => unreachable,
        };

        return act;
    }

    pub fn is(s: Self, comptime expected: ValueTag) bool {
        return tag(s) == expected;
    }

    pub fn isFalsey(s: Self) bool {
        return switch (s) {
            .Nil => true,
            .Bool => |b| !b,
            else => false,
        };
    }
    pub fn format(s: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (s) {
            .Obj => |o| {
                if (o.is(lx.String)) {
                    try writer.print("\"{s}\"", .{o.as(lx.String).chars});
                } else {
                    try writer.print("{any}", .{s});
                }
            },
            else => {
                try writer.print("{any}", .{s});
            },
        }
    }
};
