"""
Advent of Code 2024 - Day 7: Expression Evaluation Puzzle.

This puzzle involves finding valid mathematical expressions that evaluate to target values
using three operators: addition (+), multiplication (*), and concatenation (|).
"""

from dataclasses import dataclass, field
from itertools import product
from typing import Literal

OperatorType = Literal["+", "*", "|"]
VALID_OPERATORS: tuple[OperatorType, ...] = ("+", "*", "|")


@dataclass
class ExpressionPuzzle:
    """Represents a single expression puzzle with a target value and sequence of numbers."""

    target_value: int
    sequence: list[int]
    possible_expressions: list[tuple[str, ...]] = field(init=False)
    evaluated_results: list[int] = field(init=False)
    has_solution: bool = field(init=False, default=False)

    def __post_init__(self) -> None:
        """Initialize possible expressions and evaluate results."""
        self.possible_expressions = list(
            product(VALID_OPERATORS, repeat=len(self.sequence) - 1)
        )
        self.evaluated_results = self._evaluate_all_expressions()
        self.has_solution = self.target_value in self.evaluated_results

    def _evaluate_all_expressions(self) -> list[int]:
        """Evaluate all possible expressions using the number sequence."""
        return [
            self._evaluate_single_expression(self.sequence, operators)
            for operators in self.possible_expressions
        ]

    @staticmethod
    def _evaluate_single_expression(
        numbers: list[int], operators: tuple[str, ...]
    ) -> int:
        """Evaluate a single expression with given numbers and operators."""

        def apply_operator(a: int, b: int, operator: OperatorType) -> int:
            match operator:
                case "+":
                    return a + b
                case "*":
                    return a * b
                case "|":
                    return int(f"{a}{b}")
                case _:
                    msg = f"Unknown operator: {operator}"
                    raise ValueError(msg)

        numbers_copy = numbers.copy()
        operators_list = list(operators)

        while len(numbers_copy) > 1:
            left = numbers_copy.pop(0)
            right = numbers_copy.pop(0)
            operator = operators_list.pop(0)
            result = apply_operator(left, right, operator)
            numbers_copy.insert(0, result)

        return numbers_copy[0]


def parse_puzzle_line(line: str) -> ExpressionPuzzle:
    """Parse a single line into an ExpressionPuzzle instance."""
    target_str, sequence_str = line.split(":")
    target = int(target_str)
    sequence = [int(num) for num in sequence_str.strip().split()]

    return ExpressionPuzzle(target_value=target, sequence=sequence)


def load_puzzles(filepath: str) -> list[ExpressionPuzzle]:
    """Load all puzzles from the input file."""
    with open(filepath) as file:
        return [parse_puzzle_line(line.strip()) for line in file]


def calculate_solution_sum(puzzles: list[ExpressionPuzzle]) -> int:
    """Calculate the sum of target values for puzzles with valid solutions."""
    return sum(
        puzzle.target_value for puzzle in puzzles if puzzle.has_solution
    )


def solve_puzzle_part1(filepath: str) -> int:
    """Solve part 1 of the puzzle (only + and * operators)."""
    global VALID_OPERATORS
    VALID_OPERATORS = ("+", "*")
    puzzles = load_puzzles(filepath)
    return calculate_solution_sum(puzzles)


def solve_puzzle_part2(filepath: str) -> int:
    """Solve part 2 of the puzzle (includes concatenation operator |)."""
    global VALID_OPERATORS
    VALID_OPERATORS = ("+", "*", "|")
    puzzles = load_puzzles(filepath)
    return calculate_solution_sum(puzzles)


if __name__ == "__main__":
    input_filepath = "inputs.txt"

    # Solve Part 1
    result_part1 = solve_puzzle_part1(input_filepath)
    print(f"Part 1 Solution: {result_part1}")

    # Solve Part 2
    result_part2 = solve_puzzle_part2(input_filepath)
    print(f"Part 2 Solution: {result_part2}")
