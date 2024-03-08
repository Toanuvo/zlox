const std = @import("std");
const Allocator = std.mem.Allocator;
const lx = @import("zlox.zig");
const GCopts = @import("GCopts");

pub const Obj = @This();

tp: Type,
next: ?*Obj = null,
isMarked: bool = false,

pub inline fn as(s: *Obj, comptime t: type) *t {
    return @fieldParentPtr(t, "obj", s);
}

pub fn format(s: *const Obj, comptime _: []const u8, _: std.fmt.FormatOptions, stream: anytype) !void {
    try stream.print("{{{any}, next: {?*}}}", .{ s.tp, s.next });
}

pub fn from(comptime T: Type) type {
    return @field(Types, @tagName(T));
}

pub fn is(s: *Obj, comptime t: type) bool {
    return cast(t) == s.tp;
}

pub fn cast(comptime T: type) Type {
    const name = comptime b: {
        const qualifiedName = @typeName(T);
        const lastDot = std.mem.lastIndexOfScalar(u8, qualifiedName, '.').?;
        break :b qualifiedName[lastDot + 1 ..];
    };
    return std.enums.nameCast(Type, name);
}
pub const Type = std.meta.DeclEnum(Types);

fn StringContext(comptime K: type) type {
    return struct {
        pub fn hash(_: @This(), k: K) u64 {
            return k.hv;
        }

        pub fn eql(_: @This(), a: K, b: K) bool {
            return a.hv == b.hv;
        }
    };
}

pub const StringSet = std.HashMap(lx.String, void, StringContext(lx.String), 75);
pub fn Table(comptime T: type) type {
    return std.HashMap(*lx.String, T, StringContext(*lx.String), 75);
}

pub const Types = struct {
    pub const String = struct {
        obj: Obj = undefined,
        chars: []u8,
        hv: u64,

        const Self = @This();

        pub fn hash(s: []const u8) u64 {
            var h: u64 = 2166136261;
            for (s) |c| {
                h ^= c;
                h +%= 16777619;
            }
            return h;
        }

        fn ofHash(h: u64) String {
            return .{
                .hv = h,
                .chars = undefined,
            };
        }

        pub fn init(
            s: *Self,
            gc: *lx.GC,
            args: struct {
                str: []const u8,
                canTake: bool = false,
            },
        ) !?*Self {
            const str = args.str;
            const canTake = args.canTake;
            const h = hash(str);
            var hh = ofHash(h);
            const hp = &hh;

            // todo protect string while inserting into intern table
            if (gc.strings.getKeyPtr(hp)) |string| {
                if (canTake) gc.allocator().free(str);
                gc.allocator().destroy(s);
                return string.*;
            } else {
                s.* = .{
                    .hv = h,
                    .chars = if (canTake) @constCast(str) else try gc.allocator().dupe(u8, str),
                };

                try gc.strings.putNoClobber(s, {});
                return null;
            }
        }

        pub fn destroy(s: *Self, alloc: Allocator) void {
            //alloc.free(s.chars); // all strings are interned so this object never owns the string
            alloc.destroy(s);
        }
        pub fn format(s: *Self, comptime _: []const u8, _: std.fmt.FormatOptions, stream: anytype) !void {
            try stream.print("{{\"{s}\"}}", .{s.chars});
        }
    };

    pub const Func = struct {
        obj: Obj = undefined,
        arity: u8 = 0,
        upValueCount: u8 = 0,
        chunk: *lx.Chunk,
        name: ?*String = null, // fn with no name is a script

        const Self = @This();
        pub fn init(
            s: *Self,
            gc: *lx.GC,
            _: void,
        ) !?*Self {
            s.* = .{
                .chunk = try lx.Chunk.initAlloc(gc.allocator()),
            };

            return null;
        }
    };

    pub const NativeFn = struct {
        obj: Obj = undefined,
        native: NativeFnT,

        pub const NativeFnT = *const fn (u8, []lx.Value) lx.Value;

        const Self = @This();
        pub fn init(
            s: *Self,
            _: *lx.GC,
            func: NativeFnT,
        ) !*Self {
            s.* = .{
                .native = func,
            };
            return null;
        }
    };

    pub const Closure = struct {
        obj: Obj = undefined,
        func: *Func,
        upValues: []*Upvalue,

        const Self = @This();
        pub fn init(
            s: *Self,
            gc: *lx.GC,
            func: *Func,
        ) !?*Self {
            s.* = .{
                .func = func,
                .upValues = try gc.allocator().alloc(*Upvalue, func.upValueCount),
            };
            return null;
        }
    };

    pub const Upvalue = struct {
        obj: Obj = undefined,
        location: *lx.Value,
        closed: lx.Value = .Nil,
        next: ?*Upvalue = null,

        const Self = @This();
        pub fn init(
            s: *Self,
            _: *lx.GC,
            slot: *lx.Value,
        ) !?*Self {
            s.* = .{
                .location = slot,
            };
            return null;
        }
    };
};
