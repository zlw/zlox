const std = @import("std");
const Object = @import("./object.zig").Object;

pub const Value = union(enum) {
    const Self = @This();

    nil,
    boolean: bool,
    number: f64,
    object: *Object,

    pub inline fn BooleanValue(val: bool) Self {
        return Value{ .boolean = val };
    }

    pub inline fn NumberValue(val: f64) Self {
        return Value{ .number = val };
    }

    pub inline fn ObjectValue(obj: *Object) Self {
        return Value{ .object = obj };
    }

    pub inline fn NilValue() Self {
        return Value.nil;
    }

    pub fn equal(self: Self, other: Self) bool {
        return switch (self) {
            .nil => other == .nil,
            .boolean => other == .boolean and self.boolean == other.boolean,
            .number => other == .number and self.number == other.number,
            .object => other == .object and {
                const a = self.object.asString();
                const b = other.object.asString();

                return a.length == b.length and std.mem.eql(u8, a.chars, b.chars);
            },
        };
    }
};

pub fn printValue(boxed: Value) void {
    switch (boxed) {
        .number => |value| std.debug.print("{}", .{value}),
        .boolean => |value| std.debug.print("{}", .{value}),
        .nil => std.debug.print("nil", .{}),
        .object => |value| switch (value.objectType) {
            .String => std.debug.print("{s}", .{value.asString().chars}),
        },
    }
}
