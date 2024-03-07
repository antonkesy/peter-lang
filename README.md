# peter-lang

Useless and barely functional toy programming language written in Haskell

```
void main() {
  int i = 1;
  int k = i + 1;
}
```

## Syntax

C-Style

```
void main() {
  print("Hello, World!");
}
```

## Usage

```bash
> stack run -- --help
Usage: peter-exe [-i|--inline STRING] [PATH]

Available options:
  -i,--inline STRING       Inline string to parse and interpret
  -h,--help                Show this help text
```

```bash
stack run -- ./examples/main_hello_world.mmm
stack run -- -i "int main() { print(\"Hello, World\"); }"

stack run -- ./examples/short_hello_world.mmm
stack run -- -i "print(\"Hello, World\");"
```

### Tests

`stack test`

## Formatting

[ormolu](https://github.com/tweag/ormolu)

```
ormolu --mode inplace $(find . -name '*.hs')
```

## CI

`docker build .`
