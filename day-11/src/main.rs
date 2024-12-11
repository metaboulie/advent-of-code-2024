use std::collections::HashMap;
use std::time::Instant;

#[derive(Clone)]
struct Evaluation {
    number: i64,
    num_blink: i32,
    factor: f64,
}

impl Evaluation {
    #[inline(always)]
    fn new(number: i64, num_blink: i32, factor: f64) -> Self {
        Self { number, num_blink, factor }
    }
}

#[inline(always)]
fn evaluation_loop(mut evaluations: Vec<Evaluation>, num_blink: i32) -> Vec<Evaluation> {
    evaluations.reserve(evaluations.len() * 2);
    for _ in 0..num_blink {
        evaluations = get_children_evaluations(evaluations);
    }
    evaluations
}

#[inline]
fn get_children_evaluations(evaluations: Vec<Evaluation>) -> Vec<Evaluation> {
    let estimated_size = evaluations.len() * 2;
    let mut children = Vec::with_capacity(estimated_size);
    
    for eval in evaluations {
        children.extend(child_evaluation(eval));
    }
    
    combine_evaluations(children)
}

#[inline]
fn combine_evaluations(evaluations: Vec<Evaluation>) -> Vec<Evaluation> {
    let mut combined = HashMap::with_capacity(evaluations.len());
    
    for eval in evaluations {
        let key = (eval.number, eval.num_blink);
        combined.entry(key)
            .and_modify(|e| *e += eval.factor)
            .or_insert(eval.factor);
    }

    let mut result = Vec::with_capacity(combined.len());
    for ((number, num_blink), factor) in combined {
        result.push(Evaluation::new(number, num_blink, factor));
    }
    result
}

#[inline]
fn child_evaluation(eval: Evaluation) -> Vec<Evaluation> {
    let Evaluation { number, num_blink, factor } = eval;
    
    if number == 0 {
        return vec![Evaluation::new(1, num_blink - 1, factor)];
    }

    if number < 10 {
        return vec![Evaluation::new(number * 2024, num_blink - 1, factor)];
    }

    let mut n = number;
    let mut digit_count = 0;
    while n > 0 {
        digit_count += 1;
        n /= 10;
    }

    if digit_count % 2 == 0 {
        let divisor = 10_i64.pow((digit_count / 2) as u32);
        let second_half = number % divisor;
        let first_half = number / divisor;
        
        vec![
            Evaluation::new(first_half, num_blink - 1, factor),
            Evaluation::new(second_half, num_blink - 1, factor),
        ]
    } else {
        vec![Evaluation::new(number * 2024, num_blink - 1, factor)]
    }
}

fn run_calculation() -> f64 {
    let num_blink = 75;
    let initial_numbers = vec![41078, 18, 7, 0, 4785508, 535256, 8154, 447];
    
    let mut initial_evaluations = Vec::with_capacity(initial_numbers.len());
    for &number in &initial_numbers {
        initial_evaluations.push(Evaluation::new(number, num_blink, 1.0));
    }

    evaluation_loop(initial_evaluations, num_blink)
        .into_iter()
        .fold(0.0, |acc, eval| acc + eval.factor)
}

fn main() {
    let mut times = Vec::with_capacity(10);
    
    for _ in 0..10 {
        let start = Instant::now();
        run_calculation();
        let duration = start.elapsed();
        
        times.push(duration.as_micros());
    }
    
    times.sort_unstable();
    let median = if times.len() % 2 == 0 {
        (times[times.len()/2 - 1] + times[times.len()/2]) as f64 / 2.0
    } else {
        times[times.len()/2] as f64
    };
    
    println!("\nMedian execution time: {:.2} ms", median / 1000.0);
}
