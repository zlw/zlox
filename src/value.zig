const std = @import("std");

pub const Value = union(enum) {
    const Self = @This();

    nil,
    boolean: bool,
    number: f64,

    pub inline fn BooleanValue(val: bool) Self {
        return Value{ .boolean = val };
    }

    pub inline fn NumberValue(val: f64) Self {
        return Value{ .number = val };
    }

    pub inline fn NilValue() Self {
        return Value{ .nil = null };
    }
};

pub fn printValue(boxed: Value) void {
    switch (boxed) {
        .number => |value| std.debug.print("{}", .{value}),
        .boolean => |value| std.debug.print("{}", .{value}),
        .nil => std.debug.print("nil", .{}),
    }
}
