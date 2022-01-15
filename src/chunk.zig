const std = @import("std");
const DynamicArray = @import("./dynamic_array.zig").DynamicArray;
const Value = @import("./value.zig").Value;

const Allocator = std.mem.Allocator;

const expect = std.testing.expect;

pub const OpCode = enum(u8) {
    const Self = @This();

    op_constant,
    op_return,

    pub fn toU8(self: Self) u8 {
        return @enumToInt(self);
    }
};

pub const Chunk = struct {
    const Self = @This();

    const BytesArray = DynamicArray(u8);
    const ValueArray = DynamicArray(Value);

    code:      BytesArray,
    constants: ValueArray,

    pub fn init(allocator: *Allocator) Chunk {
        return Self{
            .code = BytesArray.init(allocator),
            .constants = ValueArray.init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Self, byte: u8) void {
        self.code.appendItem(byte);
    }

    pub fn addConstant(self: *Self, value: Value) u16 {
        self.constants.appendItem(value);
        return @intCast(u16, self.constants.count - 1);
    }
};

test "create a Chunk with bytes only" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked) expect(false) catch @panic("The list is leaking");
    }

    var chunk = Chunk.init(&gpa.allocator());
    defer chunk.deinit();

    chunk.write(OpCode.op_return.toU8());
    try expect(chunk.code.items[0] == OpCode.op_return.toU8());

    chunk.write(OpCode.op_return.toU8());
    chunk.write(OpCode.op_return.toU8());
    chunk.write(OpCode.op_return.toU8());
    chunk.write(OpCode.op_return.toU8());
    chunk.write(OpCode.op_return.toU8());

    try expect(chunk.code.items[4] == OpCode.op_return.toU8());
    chunk.deinit();
}
