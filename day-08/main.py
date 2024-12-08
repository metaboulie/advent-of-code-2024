"""
Advent of Code 2024, Day 8: Frequency Planning.

This solution maps radio frequencies in a city grid where:
- Each letter represents an antenna broadcasting on a specific frequency
- Antinodes are formed in straight lines between antennas of the same frequency
- The goal is to calculate the total number of unique antinode positions
"""

from collections import defaultdict
from dataclasses import dataclass, field
from itertools import combinations

GridPosition = tuple[int, int]


@dataclass
class FrequencyGroup:
    """Group of antennas broadcasting on the same frequency."""

    antenna_positions: list[GridPosition] = field(
        init=False, default_factory=list
    )
    frequency_id: str = field(init=False, default="")
    interference_points: set[GridPosition] = field(
        init=False, default_factory=set
    )

    def calculate_interference_points(
        self, grid_width: int, grid_height: int
    ) -> None:
        """
        Calculate all interference points for antennas on the same frequency.

        Args:
            grid_width: The width of the radio grid
            grid_height: The height of the radio grid

        """

        def is_within_grid(x: int, y: int) -> bool:
            """
            Check if a point lies within the grid boundaries.

            Args:
                x: X-coordinate to check
                y: Y-coordinate to check

            Returns:
                bool: True if the point is within grid boundaries

            """
            return 0 <= x < grid_width and 0 <= y < grid_height

        def extend_interference_line(
            start: GridPosition, end: GridPosition
        ) -> set[GridPosition]:
            """
            Calculate all interference points in a line extending from start through end.

            Args:
                start: Starting antenna position
                end: Ending antenna position

            Returns:
                set[GridPosition]: Set of all interference points along the line

            """
            points = {start, end}
            dx, dy = end[0] - start[0], end[1] - start[1]
            current = end

            while is_within_grid(current[0] + dx, current[1] + dy):
                current = (current[0] + dx, current[1] + dy)
                points.add(current)
            return points

        # Calculate interference points between each pair of antennas
        antenna_pairs = list(combinations(self.antenna_positions, 2))
        for pos1, pos2 in antenna_pairs:
            self.interference_points |= extend_interference_line(pos1, pos2)
            self.interference_points |= extend_interference_line(pos2, pos1)


@dataclass
class FrequencyMap:
    """Manages all frequency groups and their interference patterns."""

    frequency_groups: dict[str, FrequencyGroup] = field(init=False)
    total_interference_points: set[GridPosition] = field(init=False)

    def __init__(self) -> None:
        """Initialize an empty frequency map."""
        self.frequency_groups = defaultdict(FrequencyGroup)
        self.total_interference_points = set()

    def calculate_all_interference(
        self, grid_width: int, grid_height: int
    ) -> None:
        """
        Calculate interference points for all frequency groups.

        Args:
            grid_width: The width of the radio grid
            grid_height: The height of the radio grid

        """
        for frequency_group in self.frequency_groups.values():
            frequency_group.calculate_interference_points(
                grid_width, grid_height
            )
            self.total_interference_points |= (
                frequency_group.interference_points
            )


@dataclass
class RadioGrid:
    """Represents the city's radio frequency grid."""

    frequency_map: FrequencyMap = field(init=False)
    width: int = field(init=False, default=0)
    height: int = field(init=False, default=0)

    def __init__(self, grid_data: list[str]) -> None:
        """
        Initialize the radio grid from input data.

        Args:
            grid_data: List of strings representing the grid layout where:
                      - '.' represents empty space
                      - Letters represent antennas of different frequencies

        """
        self.frequency_map = FrequencyMap()
        self.height, self.width = len(grid_data), len(grid_data[0])

        for row, line in enumerate(grid_data):
            for col, frequency in enumerate(line):
                if frequency != ".":
                    self.frequency_map.frequency_groups[
                        frequency
                    ].antenna_positions.append((row, col))

    def get_total_interference_count(self) -> int:
        """
        Get the total number of unique interference points across all frequencies.

        Returns:
            int: Count of unique interference points

        """
        return len(self.frequency_map.total_interference_points)


def solve_frequency_planning(input_path: str) -> None:
    """
    Solve the frequency planning puzzle by calculating total interference points.

    Args:
        input_path: Path to the input file containing the grid data

    """
    with open(input_path) as f:
        grid_data = [line.strip() for line in f]

    radio_grid = RadioGrid(grid_data)
    radio_grid.frequency_map.calculate_all_interference(
        radio_grid.height, radio_grid.width
    )
    print(
        f"Total interference points: {radio_grid.get_total_interference_count()}"
    )


if __name__ == "__main__":
    solve_frequency_planning("inputs.txt")
