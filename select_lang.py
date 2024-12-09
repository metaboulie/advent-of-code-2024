import random


languages = [
    "Swift", "Dart",
    "Elixir", "Crystal", "F#",
    "Clojure", "Elm", "Reason", "Nim",
    "V", "Solidity", "Chapel", "Idris",
    "Lua", "Perl 6 (Raku)", "Red",
    "Groovy", "Mojo", "Carbon", "Scala"
]

selected_langs = [
    "Kotlin", "OCaml", "Rust", "Julia"
]


def main(languages: list[str] = languages) -> None:
    print(random.choice(languages))


if __name__ == "__main__":
    main()
