## Synopsis

Confero is an OCaml collation library, implementing the [Unicode Collation
Algorithm (UCA)][uca] including the Default Unicode Collation Element Table
(DUCET).

## Usage

```ocaml
# #require "confero.ducet";;
# Confero.collate "Ångstrom 2" "angström 1";;
- : int = 1
# Confero.collate "Ångstrom 1" "angström 2";;
- : int = -1
```

[uca]: https://unicode.org/reports/tr10/
