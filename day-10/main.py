from dataclasses import dataclass, field
from typing import TypedDict


class Position(TypedDict):
    """Represents a 2D position with x and y coordinates."""

    x: int
    y: int


def hash_dict(d: dict) -> int:
    """Create a hashable representation of a dictionary."""
    return hash(tuple(sorted(d.items())))


@dataclass
class Map:
    """Represents a numeric grid map with trailheads and path-finding capabilities."""

    trailheads: list[Position] = field(init=True)
    data: list[list[int]] = field(init=True)

    def trail(self) -> int:
        """Calculate the total number of valid paths from all trailheads."""
        total_num = 0
        for head in self.trailheads:
            target_number = 1
            paths: list[Position] = [head]

            while target_number < 10:
                new_paths: list[Position] = []
                for path in paths:
                    next_positions = self.search_around(path, target_number)
                    new_paths.extend(next_positions)
                paths = new_paths[:]
                if not paths:
                    break
                target_number += 1

            # total_num += len(set(hash_dict(p) for p in paths))  # part 1
            total_num += len(paths)  # part 2
        return total_num

    def search_around(
        self, current_position: Position, target_number: int
    ) -> list[Position]:
        """Search for valid next positions around the current position."""
        positions = []
        directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

        for dx, dy in directions:
            next_position = Position(
                x=current_position["x"] - dx, y=current_position["y"] - dy
            )

            if not (
                0 <= next_position["x"] < len(self.data[0])
                and 0 <= next_position["y"] < len(self.data)
            ):
                continue

            if (
                self.data[next_position["y"]][next_position["x"]]
                == target_number
            ):
                positions.append(next_position)

        return positions


def parse_input(file_name: str) -> tuple[list[list[int]], list[Position]]:
    """Parse the input file into a grid and list of trailheads."""
    with open(file_name, "r") as f:
        lines = [line.strip() for line in f.readlines()]

    data = []
    trailheads: list[Position] = []

    for i, row in enumerate(lines):
        line = []
        for j, number in enumerate(row):
            number = int(number)
            line.append(number)
            if number == 0:
                trailheads.append(Position(x=j, y=i))
        data.append(line)

    return data, trailheads


def main(file_name: str) -> None:
    """Main function to process the input and print the result."""
    data, trailheads = parse_input(file_name)
    map_obj = Map(
        trailheads=trailheads, data=data
    )  # Renamed variable to avoid shadowing
    trail_num = map_obj.trail()
    print(trail_num)


if __name__ == "__main__":
    main("inputs.txt")
