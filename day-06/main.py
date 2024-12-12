from __future__ import annotations

from collections import namedtuple
from dataclasses import dataclass, field
from enum import Enum


Position = tuple[int, int]


class Direction(Enum):
    NORTH = 0
    EAST = 1
    SOUTH = 2
    WEST = 3

    def turn_right(self):
        return Direction((self.value + 1) % 4)


GuardState = namedtuple("GuardState", ["x", "y", "direction"])


class Guard:
    state: GuardState
    visited_positions: list[Position] = list()

    def __init__(self, x: int, y: int, direction: Direction) -> None:
        self.state = GuardState(x=x, y=y, direction=direction)
        self.visited_positions = list()

    def in_loop(self, map: Map) -> bool:
        past_states = [self.state]
        while self.next_position_in_map(map):
            self.move_in_map(map)
            if self.state in past_states:
                return True
            past_states.append(self.state)
        return False

    def next_position_in_map(self, map: Map) -> bool:
        next_position = self.forward_position
        if 0 <= next_position[0] < map.x_lim and 0 <= next_position[0] < map.y_lim:
            return True
        return False

    def move_in_map(self, map: Map, record_trace: bool = False) -> None:
        next_position = self.forward_position
        if next_position in map.obstacles:
            self.turn_right()
        else:
            self.step()
            if record_trace and next_position not in self.visited_positions:
                self.visited_positions.append(next_position)

    def step(self) -> None:
        x, y = self.forward_position
        self.state = GuardState(x=x, y=y, direction=self.state.direction)

    def turn_right(self) -> None:
        direction = self.state.direction.turn_right()
        self.state = GuardState(x=self.state.x, y=self.state.y, direction=direction)

    @property
    def forward_position(self) -> Position:
        movements = {
            Direction.NORTH: (0, -1),
            Direction.EAST: (1, 0),
            Direction.SOUTH: (0, 1),
            Direction.WEST: (-1, 0),
        }
        dx, dy = movements[self.state.direction]

        return self.state.x + dx, self.state.y + dy

    def clone(self) -> Guard:
        return Guard(**self.state._asdict())


@dataclass
class Map:
    x_lim: int = field(init=True)
    y_lim: int = field(init=True)
    obstacles: list[Position] = field(init=True)

    def clone(self) -> Map:
        return Map(x_lim=self.x_lim, y_lim=self.y_lim, obstacles=self.obstacles[:])


def part_one(guard: Guard, map: Map) -> int:
    while guard.next_position_in_map(map):
        guard.move_in_map(map, record_trace=True)
    return len(guard.visited_positions)


def part_two(guard: Guard, map: Map) -> int:
    acc = 0
    fake_obstacles: list[Position] = list()
    init_position = (guard.state.x, guard.state.y)
    while guard.next_position_in_map(map):
        guard_clone = guard.clone()
        next_position = guard.forward_position
        fake_map = map.clone()
        fake_map.obstacles.append(guard.forward_position)
        if (
            next_position != init_position
            and (next_position not in fake_obstacles)
            and guard_clone.in_loop(fake_map)
        ):
            fake_obstacles.append(guard.forward_position)
        guard.move_in_map(map)
        acc += 1
        print(acc)
    return len(fake_obstacles)


def parse_input(file_name: str) -> (Map, Guard):
    with open(file_name, "r") as f:
        lines = [line.strip() for line in f.readlines()]

    obstacles: list[Position] = list()
    for i, row in enumerate(lines):
        for j, symbol in enumerate(row):
            if symbol == "#":
                obstacles.append((j, i))
            if symbol == "^":
                guard = Guard(x=j, y=i, direction=Direction.NORTH)
    x_lim, y_lim = len(lines), len(lines[0])
    map = Map(x_lim=x_lim, y_lim=y_lim, obstacles=obstacles)
    return map, guard


def main(file_name: str):
    map, guard = parse_input(file_name)
    answer_one = part_one(guard.clone(), map)
    state = GuardState(x=1, y=2, direction=Direction.NORTH)
    states = [
        GuardState(x=1, y=2, direction=Direction.NORTH),
        GuardState(x=2, y=2, direction=Direction.NORTH),
    ]
    print(state in states)
    print(answer_one)
    answer_two = part_two(guard.clone(), map)
    print(answer_two)


if __name__ == "__main__":
    main("inputs.txt")
