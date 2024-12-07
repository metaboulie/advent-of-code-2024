"""Advent of code 2024. Day 7."""


from dataclasses import dataclass, field
from itertools import product
from typing import Literal

Operator = Literal["+", "*", "|"]


@dataclass
class Row:
    """Row data."""

    calibration: int = field(init=True)
    numbers: list[int] = field(init=True)
    operator_combinations: list[tuple[str]] = field(init=False)
    equation_results: list[int] = field(init=False)
    result: bool = field(init=False, default=False)

    def __post_init__(self) -> None:
        """Post calculation."""
        self.operator_combinations = list(
            product("+*|", repeat=len(self.numbers) - 1)
        )
        self.equation_results = self.get_equation_result()

        self.result = any(
            result == self.calibration
            for result in self.equation_results
        )

    def get_equation_result(self) -> list[int]:
        def calculate(a: int, b: int, operator: Operator) -> int:
            match operator:
                case "+":
                    return a + b
                case "*":
                    return a * b
                case "|":
                    return int(str(a) + str(b))
                case _:
                    return a + b
        def calculate_result(numbers: list[int], operators: list[Operator]) -> int:
            numbers_, operators_ = numbers[:], list(operators)
            while len(numbers_) > 1:
                a, b, operator = numbers_.pop(0), numbers_.pop(0), operators_.pop(0)
                c = calculate(a, b, operator)
                numbers_.insert(0, c)
            return numbers_[0]
        return [calculate_result(self.numbers, operators) for operators in self.operator_combinations]


def get_row_from_line(line: str) -> Row:
    calibration, numbers = line.split(":")
    numbers = [int(number) for number in numbers.strip().split(" ")]
    calibration = int(calibration)

    return Row(calibration=calibration, numbers=numbers)


def read_file(file: str) -> list[Row]:
    with open(file) as f:
        lines = f.readlines()
    return list(map(get_row_from_line, lines))


def sum_result(data: list[Row]) -> int:
    """Sum the result."""
    return sum([row.calibration for row in data if row.result])


def main(file_name: str) -> None:
    rows = read_file(file_name)
    print(sum_result(rows))

if __name__ == "__main__":
    file_name = "inputs.txt"
    main(file_name)
