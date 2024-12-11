import statistics
import timeit
from typing import NamedTuple


class Evaluation(NamedTuple):
    # Represents an evaluation state with:
    # - number: the current number being processed
    # - num_blink: remaining blink operations
    # - factor: the number of times this state has been encountered
    number: int
    num_blink: int
    factor: float


def evaluation_loop(evaluations: list[Evaluation], num_blink: int) -> list[Evaluation]:
    # Main loop that processes evaluations for the specified number of blinks
    # Each iteration transforms the current evaluations into their children
    for _ in range(num_blink):
        evaluations = get_children_evaluations(evaluations)
    return evaluations


def get_children_evaluations(evaluations: list[Evaluation]):
    # Generates all possible child evaluations from the current set
    # Flattens the list of children and combines duplicate states
    children = [child for eval in evaluations for child in child_evaluation(eval)]
    return combine_evaluations(children)


def combine_evaluations(evaluations: list[Evaluation]) -> list[Evaluation]:
    # Combines evaluations that have the same number and num_blink
    # by adding their factors together
    combined = {}
    for number, num_blink, factor in evaluations:
        key = (number, num_blink)
        combined[key] = combined.get(key, 0) + factor
    return [Evaluation(*key, factor) for key, factor in combined.items()]


def child_evaluation(eval: Evaluation) -> list[Evaluation]:
    # Generates the next possible states for a single evaluation
    # Rules:
    # 1. If number is 0, transforms to 1
    # 2. If number has even digits, splits into two numbers
    # 3. If number has odd digits, multiplies by 2024
    number, num_blink, factor = eval

    if number == 0:
        return [Evaluation(1, num_blink - 1, factor)]

    num_str = str(number)
    if len(num_str) % 2 == 0:
        mid = len(num_str) // 2
        return [
            Evaluation(int(num_str[:mid]), num_blink - 1, factor),
            Evaluation(int(num_str[mid:]), num_blink - 1, factor),
        ]
    return [Evaluation(number * 2024, num_blink - 1, factor)]


if __name__ == "__main__":
    num_blink = 75
    initial_numbers = [41078, 18, 7, 0, 4785508, 535256, 8154, 447]
    initial_evaluations = [
        Evaluation(number, num_blink, 1) for number in initial_numbers
    ]

    def run_evaluation():
        return sum(
            eval.factor for eval in evaluation_loop(initial_evaluations, num_blink)
        )

    # Time the execution of the run_evaluation function 10 times
    times = timeit.repeat(run_evaluation, repeat=10, number=1)

    # Calculate the median of the recorded times
    median_time = statistics.median(times)

    print(f"Median execution time over 10 runs: {median_time:.6f} seconds")
