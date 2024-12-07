## Tools installed

-   java
-   kotlin
-   gradle
-   openjdk

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

Run the project

```fish
./gradlew run
```
