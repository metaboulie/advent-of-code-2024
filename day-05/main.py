"""
Puzzle Solution Strategy:

1. Parse input into two sections:
   - Section 1: Rules defining valid number pairs in format "a|b" (a comes before b)
   - Section 2: Lists of numbers to validate/sort.

2. For each number list:
   - Check if numbers are in correct order by comparing adjacent pairs
   - If pair (b,a) exists in rules, the order is invalid
   - Track the middle number for valid sequences

3. Part 1: Sum middle numbers of sequences already in correct order
4. Part 2: Sort invalid sequences according to rules and sum their middle numbers
"""

from dataclasses import dataclass, field


@dataclass
class OrderingRules:
    """Stores valid ordering rules as a set of 'a|b' strings."""

    valid_pairs: set[str] = field(default_factory=set)

    def __init__(self, rule_lines: list[str]) -> None:
        """
        Initialize OrderingRules with a list of rule strings.

        Args:
            rule_lines: List of strings in format 'a|b' where a comes before b

        """
        self.valid_pairs = set(rule_lines)

    def is_valid_order(self, first: int, second: int) -> bool:
        """
        Check if two numbers are in valid order according to rules.

        Args:
            first: First number in the pair to compare
            second: Second number in the pair to compare

        Returns:
            bool: True if order is valid, False otherwise

        """
        return f"{second}|{first}" not in self.valid_pairs


@dataclass
class NumberSequence:
    """Represents a sequence of numbers to be validated/sorted."""

    numbers: list[int]
    middle_number: int = field(init=False)
    is_ordered: bool = field(init=False, default=False)

    def __post_init__(self) -> None:
        """
        Initialize middle_number after instance creation.

        Sets middle_number to the middle value in the numbers list.
        """
        self.middle_number = self.numbers[len(self.numbers) // 2]

    def validate_order(self, rules: OrderingRules) -> None:
        """
        Check if sequence follows ordering rules.

        Validates each adjacent pair in the sequence against the rules.
        Sets is_ordered to True if all pairs are valid.

        Args:
            rules: OrderingRules instance containing valid number pairs

        """
        for i in range(len(self.numbers) - 1):
            if not rules.is_valid_order(self.numbers[i], self.numbers[i + 1]):
                return
        self.is_ordered = True

    def sort_according_to_rules(self, rules: OrderingRules) -> None:
        """
        Sort the sequence according to ordering rules.

        Uses bubble sort with custom comparison based on rules.
        Updates middle_number after sorting is complete.

        Args:
            rules: OrderingRules instance containing valid number pairs

        """
        while not self.is_ordered:
            # Bubble sort with custom comparison
            for i in range(len(self.numbers) - 1):
                if not rules.is_valid_order(
                    self.numbers[i], self.numbers[i + 1]
                ):
                    self.numbers[i], self.numbers[i + 1] = (
                        self.numbers[i + 1],
                        self.numbers[i],
                    )
            self.validate_order(rules)
        self.middle_number = self.numbers[len(self.numbers) // 2]


def parse_input(file_path: str) -> tuple[OrderingRules, list[NumberSequence]]:
    """
    Parse input file into rules and sequences to validate.

    Args:
        file_path: Path to input file containing rules and sequences

    Returns:
        Tuple containing:
            - OrderingRules instance with parsed rules
            - List of NumberSequence instances from input

    """
    with open(file_path) as f:
        lines = f.readlines()

    split_index = lines.index("\n")
    rule_lines = [line.strip() for line in lines[:split_index]]
    sequence_lines = [line.strip() for line in lines[split_index + 1 :]]

    rules = OrderingRules(rule_lines)
    sequences = [
        NumberSequence([int(num) for num in line.split(",")])
        for line in sequence_lines
    ]
    return rules, sequences


def solve_puzzle(file_path: str) -> tuple[int, int]:
    """
    Solve both parts of the puzzle.

    Args:
        file_path: Path to input file containing rules and sequences

    Returns:
        Tuple containing:
            - Part 1: Sum of middle numbers from already ordered sequences
            - Part 2: Sum of middle numbers after sorting unordered sequences

    """
    rules, sequences = parse_input(file_path)

    # Part 1: Find sequences already in correct order
    for seq in sequences:
        seq.validate_order(rules)
    part1_sum = sum(seq.middle_number for seq in sequences if seq.is_ordered)

    # Part 2: Sort incorrect sequences and sum middle numbers
    unordered_sequences = [seq for seq in sequences if not seq.is_ordered]
    for seq in unordered_sequences:
        seq.sort_according_to_rules(rules)
    part2_sum = sum(seq.middle_number for seq in unordered_sequences)

    return part1_sum, part2_sum


def main(file_path: str) -> None:
    """
    Run the puzzle solver and print results.

    Args:
        file_path: Path to input file containing rules and sequences

    """
    part1_result, part2_result = solve_puzzle(file_path)
    print(f"Part 1: {part1_result}")
    print(f"Part 2: {part2_result}")


if __name__ == "__main__":
    main("inputs.txt")
