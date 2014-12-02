# jsontsv

A simple tool to transform JSON into tab-separated line-oriented output
amenable to downstream Unix text processing. 

## Synopsis

input.json:

```json
{
  "title": "Terminator 2: Judgement Day",
  "year": 1991,
  "stars": [
    "Arnold Schwarzenegger",
    "Linda Hamilton"
  ],
  "ratings": {
    "imdb": 8.5
  }
}
{
  "title": "Interstellar",
  "year": 2014,
  "stars": [
    "Matthew McConaughey",
    "Anne Hathaway"
  ],
  "ratings": {
    "imdb": 8.9
  }
}
```

    jsontsv 'title year stars ratings.imdb' < input.json

Outputs this tab-separated text:

```tsv
Terminator 2: Judgement Day	1991	Arnold Schwarzenegger,Linda Hamilton	8.5
Interstellar	2014	Matthew McConaughey,Anne Hathaway	8.9
```

## Setup

From the project directory, 

    cabal install

Make sure the installed executable is on your PATH.

## Usage

Input should be a stream of JSON objects with mostly uniform keys, separated by
whitespace such as newlines. If the objects are wrapped in a JSON array at the
top level, use the `jq` tool by Stephan Dolan to unwrap the objects, e.g.: 

    curl "https://api.github.com/users/danchoi/repos" | 
        jq -M '.[]' | jsontsv 'id name owner.login'

JSON leaf values are printed as follows: 

* Strings and numbers were copied to output.
* Boolean values are output as `t` or `f`.
* null is printed as `null`
* Arrays of leaf values are concatenated into a single comma-separated string

## Nested keys

    jsontsv 'title duplicates.Rental.HD duplicates.Rental.SD' < input.json

## Using a file to designate columns:

    jsontsv -f keys  < input.json

## Concatenating fields, truncating fields, etc.

This should be done downstream using a tool like AWK.

