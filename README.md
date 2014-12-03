# jsontsv

Transforms JSON objects into tab-separated line-oriented output, which is more
convenient for downstream processing with Unix tools like `grep`, `awk`,
`diff`, etc., and for inspecting with spreadsheet programs using a CSV output
option.

## Synopsis

input.json:

```json
{
  "title": "Terminator 2: Judgement Day",
  "year": 1991,
  "stars": [
    {
      "name": "Arnold Schwarzenegger"
    },
    {
      "name": "Linda Hamilton"
    }
  ],
  "ratings": {
    "imdb": 8.5
  }
}
{
  "title": "Interstellar",
  "year": 2014,
  "stars": [
    {
      "name": "Matthew McConaughey"
    },
    {
      "name": "Anne Hathaway"
    }
  ],
  "ratings": {
    "imdb": 8.9
  }
}
```

Note that this input is not actually JSON at the top-level. It is a stream of
JSON objects. It can be fed into `jsontsv`:

    jsontsv 'title year stars.name ratings.imdb' < input.json

outputs this tab-separated text:

```tsv
Terminator 2: Judgement Day	1991	Arnold Schwarzenegger,Linda Hamilton	8.5
Interstellar	2014	Matthew McConaughey,Anne Hathaway	8.9
```

You can pick off array elements using `[i]` syntax:

    jsontsv 'title year stars[0].name' < input.json

outputs 

```tsv
Terminator 2: Judgement Day     1991    Arnold Schwarzenegger
Interstellar    2014    Matthew McConaughey
```

## Installation

Assuming you have a recent version of the [Haskell
platform](https://www.haskell.org/platform/) on your system, 

    cabal update
    cabal install jsontsv

Make sure the installed executable is on your PATH.

## Usage

```
jsontsv

Usage: jsontsv FIELDS [-a STRING] ([-c|--csv] | [-d STRING])
  Transform JSON objects to TSV

Available options:
  -h,--help                Show this help text
  -a STRING                concatentated array elem delimiter. Default to comma
  -c,--csv                 output CSV
  -d STRING                output field delimiter. Defaults to tab
```

Input should be a stream of JSON objects of the same or mostly similar shape,
separated by whitespace such as newlines. If the objects are wrapped in a JSON
array at the top level or nested inside a top-level object, use the [jq][jq]
tool by Stephan Dolan to extract an object stream, e.g.: 

[jq]:http://stedolan.github.io/jq/

    curl -s "https://api.github.com/users/danchoi/repos?type=owner&sort=created&direction=desc" \
        | jq '.[]' | jsontsv 'id name stargazers_count open_issues_count' 

outputs

    27397673        jsontsv 0       0
    26033118        ngrender        24      1
    25832026        rdoc    0       0
    24756523        treehtml        0       0
    24022588        heistexamples   0       0
    24022042        hxtexamples     0       0
    24005242        jdiff   0       0
    23997156        https-types     0       0
    22763562        podcasting      0       0
    19294791        vimscript       3       0

JSON leaf values are printed as follows: 

* Strings and numbers are copied to output.
* Boolean values are output as `t` or `f`.
* null is printed as `null`
* If the leaf value is an array, it is concatenated into a single
  comma-separated string. The delimiter can be changed with the `-a` option.

## Performing post-processing on field values

Use awk!

## Known alternatives 

* [jsawk](https://github.com/micha/jsawk) Jsawk is like awk, but for JSON. (nodejs)
* [jq][jq]
* [jsoncsv](https://github.com/gradus/jsoncsv) a json to csv library in javascript/coffeescript
* [json](https://github.com/konklone/json) A free, in-browser JSON to CSV converter.
