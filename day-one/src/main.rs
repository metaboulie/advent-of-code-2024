use std::collections::HashMap;
use std::fs;

fn parse_input_pairs(input: &str) -> (Vec<i32>, Vec<i32>) {
    input
        .lines()
        .map(|line| {
            let nums: Vec<i32> = line
                .split_whitespace()
                .map(|n| n.parse().unwrap())
                .collect();
            (nums[0], nums[1])
        })
        .unzip()
}

fn calculate_absolute_differences(xs: &[i32], ys: &[i32]) -> Vec<i32> {
    xs.iter()
        .zip(ys.iter())
        .map(|(x, y)| (x - y).abs())
        .collect()
}

fn create_frequency_map(numbers: &[i32]) -> HashMap<i32, i32> {
    let mut map = HashMap::new();
    for &num in numbers {
        *map.entry(num).or_insert(0) += 1;
    }
    map
}

fn calculate_weighted_intersection(xs: &[i32], ys: &[i32]) -> i32 {
    let source_freq_map = create_frequency_map(xs);
    let target_freq_map = create_frequency_map(ys);

    source_freq_map
        .iter()
        .filter_map(|(&k, &v)| target_freq_map.get(&k).map(|&target_v| k * v * target_v))
        .sum()
}

fn main() {
    let input = fs::read_to_string("inputs.txt").expect("Failed to read input file");
    let (source_numbers, target_numbers) = parse_input_pairs(&input);

    let mut sorted_source = source_numbers.clone();
    let mut sorted_target = target_numbers.clone();
    sorted_source.sort();
    sorted_target.sort();

    let total_difference: i32 = calculate_absolute_differences(&sorted_source, &sorted_target)
        .iter()
        .sum();

    let weighted_intersection_sum =
        calculate_weighted_intersection(&source_numbers, &target_numbers);

    println!("answer to question 1: {}", total_difference);
    println!("answer to question 2: {}", weighted_intersection_sum);
}
