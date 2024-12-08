## Day 8. Kotlin

[Tips and Tricks for Solving Advent of Code](https://blog.jetbrains.com/kotlin/2021/12/tips-and-tricks-for-solving-advent-of-code/#3.-storing-input)

## Tools installed

-   java
-   kotlin
-   gradle
-   openjdk
-   kotlin-language-server

## Commands

Configure gradle

```fish
sudo ln -sfn $(brew --prefix)/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk
set -Ux JAVA_HOME (/usr/libexec/java_home)
set -gx JAVA_HOME (/usr/libexec/java_home)
```

Initialize project with `gradle`

```fish
gradle init
```

editing in the file `/example/App.kt`

Run the project

```fish
./gradlew run
```

## My thoughts

the file structure is not minimal

```
Links  Size Date Modified Git Name
   21     -  9 Dec 02:04   -M .
    1    40  9 Dec 01:13   -- ├── .editorconfig
    1   278  7 Dec 21:30   -- ├── .gitattributes
    1   103  7 Dec 21:30   -- ├── .gitignore
    3     -  7 Dec 21:47   -- ├── .kotlin
    3     -  9 Dec 01:22   -- │   └── sessions
    1     0  9 Dec 01:22   -- │       └── kotlin-compiler-12989976077300605023.salive
    7     -  8 Dec 19:05   -- ├── .venv
    5     -  8 Dec 16:42   -M ├── app
    1 1.3Ki  8 Dec 16:42   -- │   ├── build.gradle.kts
    4     -  7 Dec 21:30   -M │   └── src
    4     -  7 Dec 21:30   -M │       ├── main
    3     -  8 Dec 16:42   -M │       │   ├── kotlin
    3     -  7 Dec 21:30   -M │       │   │   └── org
    5     -  9 Dec 01:57   -M │       │   │       └── example
    1 2.7Ki  9 Dec 01:57   -M │       │   │           ├── App.kt
    1 2.5Ki  9 Dec 01:39   -- │       │   │           ├── inputs.txt
    1   156  9 Dec 01:25   -- │       │   │           └── test.txt
    2     -  7 Dec 21:30   -- │       │   └── resources
    4     -  7 Dec 21:30   -- │       └── test
    3     -  7 Dec 21:30   -- │           ├── kotlin
    3     -  7 Dec 21:30   -- │           │   └── org
    3     -  7 Dec 21:30   -- │           │       └── example
    1   316  7 Dec 21:30   -- │           │           └── AppTest.kt
    2     -  7 Dec 21:30   -- │           └── resources
    4     -  7 Dec 21:30   -- ├── gradle
    1   337  7 Dec 21:30   -- │   ├── libs.versions.toml
    4     -  7 Dec 21:30   -- │   └── wrapper
    1  43Ki  7 Dec 21:30   -- │       ├── gradle-wrapper.jar
    1   253  7 Dec 21:30   -- │       └── gradle-wrapper.properties
    1   243  7 Dec 21:30   -- ├── gradle.properties
    1 8.6Ki  7 Dec 21:30   -- ├── gradlew
    1 2.9Ki  7 Dec 21:30   -- ├── gradlew.bat
    1 2.5Ki  8 Dec 16:37   -- ├── inputs.txt
    1  80Ki  8 Dec 20:43   -- ├── kls_database.db
    1 5.5Ki  8 Dec 20:42   -- ├── main.py
    1   486  9 Dec 02:04   -M ├── README.md
    1   891  8 Dec 18:33   -- ├── ruff.toml
    1   590  7 Dec 21:30   -- ├── settings.gradle.kts
    1   156  8 Dec 16:37   -- └── test.txt
```

maybe i will use it later
