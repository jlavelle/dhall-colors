# dhall-colors

Parser and [Dhall](https://dhall-lang.org/) code generator for the [List of colors (compact)](https://en.wikipedia.org/wiki/List_of_colors_(compact)) Wikipedia page. Inspired by [color-names](https://github.com/codebrainz/color-names), which does not appear to be maintained.

See `./output` for the generated Dhall programs.  The data is available as a list or a record. 

Each color is represented as a Dhall value of type:
```
{ name : Text
, rgb  : { r : Natural, g : Natural, b : Natural }
, hex  : Text
}
```
