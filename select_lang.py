import random


languages = [
    "Swift",
    "Crystal",
    "Elm",
    "Nim",
    "Zig",
    "Python",
    "Rust",
    "Haskell",
    "Kotlin",
]

selected_langs = ["Kotlin", "OCaml", "Rust", "Julia", "Scala", "Dart", "Elixir", "Mojo"]


def main(languages: list[str] = languages) -> None:
    print(random.choice(languages))


if __name__ == "__main__":
    main()
