# peter-lang

Useless and barely functional toy programming language interpreter written in Haskell from scratch (without any research how to actually lay things out)

The missing link between C and Python

<img src="https://m.media-amazon.com/images/M/MV5BMmQwNWY0MzMtZTgyNy00ZTM1LWI0ZDgtY2Q3NGQ4ZjhhN2U2XkEyXkFqcGdeQXVyODQyNDU4OTk@._V1_.jpg" width="720">

## Syntax

C style syntax with Python like runtime interpretation and built in functions.

### [Main](./examples/main_hello_world.mmm)

Main function `void main() {}` is the entry point.

```
void main() {
  print("Hello, World!");
}
```

Alternately, statements are executed from top to bottom in case no main is found.

```
print("Hello, World!");
```

### [Primitives](./examples/primitives.mmm)

- void
- bool
- int
- float
- str

### [Conversion](./examples/primitives.mmm)

- `str(1)` -> `"1"`
- `int("1")` -> `1`
- `float("1.3")` -> `1.3`

#### [Operations](./examples/primitives.mmm)

There is no operator precedence

| Operation | void | bool | int | float | str |
| :-------: | :--: | :--: | --- | ----- | --- |
|     +     |  x   |  x   | ✓   | ✓     | ✓   |
|     -     |  x   |  x   | ✓   | ✓     | x   |
|    \*     |  x   |  x   | ✓   | ✓     | x   |
|    \\     |  x   |  x   | ✓   | x     | x   |
|     %     |  x   |  x   | ✓   | x     | x   |
|    ==     |  x   |  ✓   | ✓   | ✓     | ✓   |
|    !=     |  x   |  ✓   | ✓   | ✓     | ✓   |
|     <     |  x   |  ✓   | ✓   | ✓     | x   |
|     >     |  x   |  ✓   | ✓   | ✓     | x   |
|    <=     |  x   |  ✓   | ✓   | ✓     | x   |
|    >=     |  x   |  ✓   | ✓   | ✓     | x   |
|    &&     |  x   |  ✓   | ✓   | x     | x   |
|   \|\|    |  x   |  ✓   | ✓   | x     | x   |

### [Flow Control](./examples/control.mmm)

If:

```
if 1 < 2 {
  i = 1;
} else {
  i = -1;
}
```

while:

```
int j = 0;
while j < 10 {
  println(str(j));
  j = j + 1;
}
```

### [Functions](./examples/functions.mmm)

```
int f() {
  return 42;
}

void main() {
  int x = f();
}
```

### [Structs](./examples/structs.mmm)

```
struct T {
  int x;
  int y;
}

T t;
t.x = 2;
```

### [Built In](./examples/hello_input.mmm)

- print -> prints string without buffering
- println -> print + appends newline at the end
- input -> reads stdin and outputs str

## Limitations & Issues

- left side of operation has to be atomic (because of [left recursive grammar](https://en.wikipedia.org/wiki/Left_recursion))
- no arrays (yet)
- many bugs
- bad error messages from parser and interpreter
- types are optional and currently not always strictly enforced

## Missing/Planned Features

- Arrays
- Classes (or something similar)
- String manipulation
- Multiple files support
- Enums
- Pattern matching
- switch
- for
- define grammar

## Installation

### Native

Dependencies:

- [stack](https://docs.haskellstack.org/en/stable/)
- ghc-9.4.7: `stack --compiler ghc-9.4.7 setup`

### Docker

```bash
docker build --target exe -t peter .
docker run peter ./examples/print.mmm
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
stack run -- -i "void main() { print(\"Hello, World\"); }"

stack run -- ./examples/short_hello_world.mmm
stack run -- -i "print(\"Hello, World\");"
```

## Tests

- `stack test`
- `cd ./test/E2E/Interpreter/examples/ && ./check_examples.sh`

## Formatting

[ormolu](https://github.com/tweag/ormolu)

```
ormolu --mode inplace $(find . -name '*.hs')
```

## CI

`docker build .` + GitHub Actions
