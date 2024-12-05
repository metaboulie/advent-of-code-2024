const std = @import("std");
const ArrayList = std.ArrayList;
const AutoHashMap = std.AutoHashMap;

// todo: should use unsigned datatype instead
pub fn parseInputPairs(allocator: std.mem.Allocator, input: []const u8) !struct { source: []i32, target: []i32 } {
    var source = ArrayList(i32).init(allocator);
    var target = ArrayList(i32).init(allocator);

    var lines = std.mem.splitScalar(u8, input, '\n');
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var numbers = std.mem.tokenizeScalar(u8, line, ' ');
        const first = numbers.next() orelse return error.InvalidInput;
        const second = numbers.next() orelse return error.InvalidInput;

        const first_num = std.fmt.parseInt(i32, first, 10) catch |err| {
            std.debug.print("Error parsing first number: {} in line: {s}\n", .{ err, line });
            return err;
        };
        const second_num = std.fmt.parseInt(i32, second, 10) catch |err| {
            std.debug.print("Error parsing second number: {} in line: {s}\n", .{ err, line });
            return err;
        };

        try source.append(first_num);
        try target.append(second_num);
    }

    return .{ .source = try source.toOwnedSlice(), .target = try target.toOwnedSlice() };
}

pub fn calculateAbsoluteDifferences(allocator: std.mem.Allocator, xs: []const i32, ys: []const i32) ![]i32 {
    var differences = std.ArrayList(i32).init(allocator);
    for (xs, ys) |x, y| {
        const diff = @abs(x - y);
        try differences.append(@intCast(diff));
    }
    return differences.toOwnedSlice();
}

pub fn createFrequencyMap(allocator: std.mem.Allocator, numbers: []const i32) !AutoHashMap(i32, i32) {
    var map = AutoHashMap(i32, i32).init(allocator);
    for (numbers) |num| {
        const entry = try map.getOrPut(num);
        if (!entry.found_existing) {
            entry.value_ptr.* = 0;
        }
        entry.value_ptr.* += 1;
    }
    return map;
}

pub fn calculateWeightedIntersection(allocator: std.mem.Allocator, xs: []const i32, ys: []const i32) !i32 {
    var source_freq_map = try createFrequencyMap(allocator, xs);
    defer source_freq_map.deinit();
    var target_freq_map = try createFrequencyMap(allocator, ys);
    defer target_freq_map.deinit();

    var sum: i32 = 0;
    var it = source_freq_map.iterator();
    while (it.next()) |entry| {
        if (target_freq_map.get(entry.key_ptr.*)) |target_freq| {
            sum += entry.key_ptr.* * entry.value_ptr.* * target_freq;
        }
    }
    return sum;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "inputs.txt", 1024 * 1024);
    defer allocator.free(input);

    const numbers = try parseInputPairs(allocator, input);
    defer allocator.free(numbers.source);
    defer allocator.free(numbers.target);

    const sorted_source = try allocator.dupe(i32, numbers.source);
    defer allocator.free(sorted_source);
    const sorted_target = try allocator.dupe(i32, numbers.target);
    defer allocator.free(sorted_target);

    std.sort.heap(i32, sorted_source, {}, std.sort.asc(i32));
    std.sort.heap(i32, sorted_target, {}, std.sort.asc(i32));

    const differences = try calculateAbsoluteDifferences(allocator, sorted_source, sorted_target);
    defer allocator.free(differences);

    var total_difference: i32 = 0;
    for (differences) |diff| {
        total_difference += diff;
    }

    const weighted_intersection_sum = try calculateWeightedIntersection(allocator, numbers.source, numbers.target);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("answer to question 1: {}\n", .{total_difference});
    try stdout.print("answer to question 2: {}\n", .{weighted_intersection_sum});
}
