const Value = @import("./value.zig").Value;
const time = @import("std").time;

pub fn clockNative(value: Value) Value {
    _ = value;    
    return Value.NumberValue(@as(f64, @floatFromInt(time.milliTimestamp())) / 1000);
}
