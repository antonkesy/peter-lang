# peter-lang

Useless and barely functional toy programming language written in Haskell

```
int main() {
  int i = 1;
  int k = i + 1;
}
```

## Running

`stack test`

## Formatting

[ormolu](https://github.com/tweag/ormolu)

```
ormolu --mode inplace $(find . -name '*.hs')
```

## CI

`docker build .`
