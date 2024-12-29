const std = @import("std");
const Object = @import("./object.zig").Object;

const nan_boxing = true;

pub const Value = if (nan_boxing) NaNBoxedValue else UnionValue;

const NaNBoxedValue = packed struct {
    const Self = @This();
    data: u64,

    const SIGN_BIT: u64 = 0x8000000000000000;
    const QNAN: u64 = 0x7ffc000000000000;

    const TAG_NIL = 1; // 01.
    const TAG_FALSE = 2; // 10.
    const TAG_TRUE = 3; // 11.

    const NIL_VAL = Self{ .data = QNAN | TAG_NIL };
    const TRUE_VAL = Self{ .data = QNAN | TAG_TRUE };
    const FALSE_VAL = Self{ .data = QNAN | TAG_FALSE };

    pub inline fn NilValue() Self {
        return NIL_VAL;
    }

    pub inline fn BooleanValue(val: bool) Self {
        return if (val) Self.TRUE_VAL else Self.FALSE_VAL;
    }

    pub inline fn NumberValue(val: f64) Self {
        return Self{ .data = @as(u64, @bitCast(val)) };
    }

    pub inline fn ObjectValue(val: *Object) Self {
        return Self{ .data = SIGN_BIT | QNAN | @intFromPtr(val) };
    }

    pub inline fn asBoolean(self: Self) bool {
        return self.data == TRUE_VAL.data;
    }

    pub inline fn asNumber(self: Self) f64 {
        return @as(f64, @bitCast(self.data));
    }

    pub inline fn asObject(self: Self) *Object {
        return @ptrFromInt(@as(usize, @intCast(self.data & ~(SIGN_BIT | QNAN))));
    }

    pub inline fn isNil(self: Self) bool {
        return self.data == NIL_VAL.data;
    }

    pub inline fn isBoolean(self: Self) bool {
        return (self.data & FALSE_VAL.data) == FALSE_VAL.data;
    }

    pub inline fn isNumber(self: Self) bool {
        return (self.data & QNAN) != QNAN;
    }

    pub inline fn isObject(self: Self) bool {
        return (self.data & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT);
    }

    pub inline fn equal(self: Self, other: Self) bool {
        // Be careful about IEEE NaN equality semantics
        if (self.isNumber() and other.isNumber()) return self.asNumber() == other.asNumber();
        return self.data == other.data;
    }

    pub inline fn isFalsey(self: Self) bool {
        if (self.isNil()) return true;
        if (self.isBoolean()) return !self.asBoolean();
        return false;
    }
};

const ValueType = enum {
    nil,
    boolean,
    number,
    object,
};

const UnionValue = union(ValueType) {
    const Self = @This();

    nil,
    boolean: bool,
    number: f64,
    object: *Object,

    pub inline fn NilValue() Self {
        return Self.nil;
    }

    pub inline fn BooleanValue(val: bool) Self {
        return Self{ .boolean = val };
    }

    pub inline fn NumberValue(val: f64) Self {
        return Self{ .number = val };
    }

    pub inline fn ObjectValue(obj: *Object) Self {
        return Self{ .object = obj };
    }

    pub inline fn asBoolean(self: Self) bool {
        return self.boolean;
    }

    pub inline fn asNumber(self: Self) f64 {
        return self.number;
    }

    pub inline fn asObject(self: Self) *Object {
        return self.object;
    }

    pub inline fn isNil(self: Self) bool {
        return self == .nil;
    }

    pub inline fn isBoolean(self: Self) bool {
        return self == .boolean;
    }

    pub inline fn isNumber(self: Self) bool {
        return self == .number;
    }

    pub inline fn isObject(self: Self) bool {
        return self == .object;
    }

    pub inline fn equal(self: Self, other: Self) bool {
        if (@as(ValueType, self) != @as(ValueType, other)) return false;

        return switch (self) {
            .nil => true,
            .boolean => self.boolean == other.boolean,
            .number => self.number == other.number,
            .object => self.object == other.object,
        };
    }

    pub inline fn isFalsey(value: Value) bool {
        return switch (value) {
            .nil => true,
            .boolean => |val| !val,
            else => false,
        };
    }
};

pub inline fn printValue(boxed: Value) void {
    const stdout = std.io.getStdOut().writer();
    const msg = "Panic while printing value printOperation\n";

    if (boxed.isNil()) {
        stdout.print("nil\n", .{}) catch @panic(msg);
    } else if (boxed.isBoolean()) {
        const value = boxed.asBoolean();
        stdout.print("{}\n", .{value}) catch @panic(msg);
    } else if (boxed.isNumber()) {
        const value = boxed.asNumber();
        stdout.print("{d}\n", .{value}) catch @panic(msg);
    } else if (boxed.isObject()) {
        const value = boxed.asObject();
        switch (value.objectType) {
            .String => stdout.print("{s}\n", .{value.asString().chars}) catch @panic(msg),
            .Function => {
                const name = if (value.asFunction().name) |name| name.chars else "script";
                stdout.print("<fn {s}>\n", .{name}) catch @panic(msg);
            },
            .NativeFunction => stdout.print("<native fn>\n", .{}) catch @panic(msg),
            .Closure => {
                const name = if (value.asClosure().function.name) |name| name.chars else "script";
                stdout.print("<fn {s}>\n", .{name}) catch @panic(msg);
            },
            .Upvalue => stdout.print("upvalue\n", .{}) catch @panic(msg),
            .Class => stdout.print("{s}\n", .{value.asClass().name.chars}) catch @panic(msg),
            .Instance => stdout.print("{s} instance\n", .{value.asInstance().class.name.chars}) catch @panic(msg),
            .BoundMethod => {
                const name = if (value.asBoundMethod().method.function.name) |name| name.chars else "script";
                stdout.print("<fn {s}>\n", .{name}) catch @panic(msg);
            },
        }
    }
}
