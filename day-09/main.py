"""Solving Part 2."""

from dataclasses import dataclass, field
from typing import TypedDict


class FreeSpace(TypedDict):
    start_index: int
    size: int


class FileBlock(TypedDict):
    start_index: int
    size: int
    fileId: int


@dataclass
class DiskMap:
    free_spaces: list[FreeSpace] = field(init=False, default_factory=list)
    file_blocks: list[FileBlock] = field(init=False, default_factory=list)
    max_free_space_size: int = field(init=False, default=0)

    def get_first_free_space(
        self, space_size: int, start_index: int
    ) -> FreeSpace | None:
        # there is no big enough free space
        if space_size > self.max_free_space_size:
            return None
        for free_space in self.free_spaces:
            # the first available free space is already behind this file
            if free_space["start_index"] >= start_index:
                return None
            if free_space["size"] >= space_size:
                return free_space
        return None

    def iterate_file_blocks(self) -> None:
        for file_block in reversed(self.file_blocks):
            free_space = self.get_first_free_space(
                file_block["size"], file_block["start_index"]
            )
            if free_space:
                file_block["start_index"] = free_space["start_index"]
                free_space["start_index"] += file_block["size"]
                free_space["size"] -= file_block["size"]

    def check_sum(self) -> int:
        return sum(
            map(
                lambda x: int(
                    x["fileId"] * (2 * x["start_index"] + x["size"] - 1) * x["size"] / 2
                ),
                self.file_blocks,
            )
        )


def get_disk_map(line: str) -> DiskMap:
    disk_map = DiskMap()
    index = 0
    max_size = 0
    for i, x in enumerate(line):
        x = int(x)
        if i % 2 == 0:
            disk_map.file_blocks.append(
                FileBlock(start_index=index, size=x, fileId=i // 2)
            )
        else:
            max_size = x if max_size < x else max_size
            disk_map.free_spaces.append(FreeSpace(start_index=index, size=x))
        index += x
    disk_map.max_free_space_size = max_size
    return disk_map


def read_file(file_name: str):
    with open(file_name, "r") as f:
        line = f.readline().strip()
    disk_map = get_disk_map(line)
    disk_map.iterate_file_blocks()
    print(disk_map.check_sum())


def main(file_name):
    read_file(file_name)


if __name__ == "__main__":
    main("inputs.txt")
