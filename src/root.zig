const std = @import("std");
const testing = std.testing;

// pub fn panic(msg: []const u8, error_return_trace: ?*builtin.StackTrace) noreturn {
//     // your implementation here
//     _ = error_return_trace;
//     std.log.debug("Error: {s}", .{msg});
//     std.os.exit(0);
//     //@compileError("panic");
// }

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}
