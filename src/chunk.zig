const std = @import("std");
const DynamicArray = @import("./dynamic_array.zig").DynamicArray;
const Value = @import("./value.zig").Value;

const Allocator = std.mem.Allocator;

const expect = std.testing.expect;

pub const OpCode = enum(u8) {
    const Self = @This();

    op_constant,
    op_negate,
    op_return,

    pub fn toU8(self: Self) u8 {
        return @enumToInt(self);
    }

    pub fn fromU8(n: u8) Self {
        return @intToEnum(Self, n);
    }

    pub fn operands(self: *Self) usize {
        return switch (self) {
            .op_constant => 1,
            else => 0,
        };
    }
};

pub const Chunk = struct {
    const Self = @This();

    const BytesArray = DynamicArray(u8);
    const ValueArray = DynamicArray(Value);
    const LinesArray = DynamicArray(usize);

    code:      BytesArray,
    constants: ValueArray,
    lines:     LinesArray,

    pub fn init(allocator: *Allocator) Chunk {
        return Self{
            .code = BytesArray.init(allocator),
            .constants = ValueArray.init(allocator),
            .lines = LinesArray.init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn write(self: *Self, byte: u8, line: usize) void {
        self.code.appendItem(byte);
        self.lines.appendItem(line);
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

    chunk.write(OpCode.op_return.toU8(), 1);
    try expect(chunk.code.items[0] == OpCode.op_return.toU8());

    chunk.write(OpCode.op_return.toU8(), 1);
    chunk.write(OpCode.op_return.toU8(), 1);
    chunk.write(OpCode.op_return.toU8(), 1);
    chunk.write(OpCode.op_return.toU8(), 1);
    chunk.write(OpCode.op_return.toU8(), 1);

    try expect(chunk.code.items[4] == OpCode.op_return.toU8());
    chunk.deinit();
}
