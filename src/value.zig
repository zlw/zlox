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

    pub inline fn isNil(self: Self) bool {
        return self == Value.nil;
    }

    pub fn equal(self: Self, other: Self) bool {
        return switch (self) {
            .nil => other == .nil,
            .boolean => other == .boolean and self.boolean == other.boolean,
            .number => other == .number and self.number == other.number,
            .object => other == .object and self.object == other.object,
        };
    }
};

pub fn printValue(boxed: Value) void {
    const stdout = std.io.getStdOut().writer();
    const msg = "Panic while printing value printOperation\n";

    switch (boxed) {
        .number => |value| stdout.print("{d}\n", .{value}) catch @panic(msg),
        .boolean => |value| stdout.print("{}\n", .{value}) catch @panic(msg),
        .nil => stdout.print("nil\n", .{}) catch @panic(msg),
        .object => |value| switch (value.objectType) {
            .String => stdout.print("{s}\n", .{value.asString().chars}) catch @panic(msg),
            .Function => {
                const name = if (value.asFunction().name) |name| name.chars else "script";
                stdout.print("<fn {s}>\n", .{name}) catch @panic(msg);
            },
            .NativeFunction => stdout.print("<native fn>\n", .{}) catch @panic(msg),
            .Closure => {
                const name = if (value.asClosure().function.name) |name| name.chars else "script";
                stdout.print("<fn {s}>\n", .{name}) catch @panic(msg);
            }
        },
    }
}
