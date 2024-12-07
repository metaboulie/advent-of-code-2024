import random


languages = [
    "Rust", "Go", "Swift", "Julia", "Dart",
    "Elixir", "Crystal", "F#", "Haskell", "Gleam",
    "Clojure", "Elm", "Reason", "Nim", "Zig",
    "V", "Solidity", "Chapel", "Idris", "Erlang",
    "Lua", "OCaml", "Perl 6 (Raku)", "Red",
    "Groovy", "Mojo", "Carbon"
]

selected_langs = [
    "Kotlin",
]


def main(languages: list[str] = languages) -> None:
    print(random.choice(languages))


if __name__ == "__main__":
    main()
