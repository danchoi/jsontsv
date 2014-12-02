# jsontsv

A simple tool to transform JSON into tab-separated line-oriented output
amenable to Unix text processing. 

## Setup

From the project directory, 

    cabal install

Make sure the installed executable is on your PATH.

## Usage

Input should be a stream of JSON objects with mostly uniform keys.

Output keys must be specified. If none are specified, all the top-level keys of
the first object are taken as the template.

    jsontsv 'title rating url' < input.json

Terminal values. If the key maps to a scalar value or Null, it is printed to
the column. 

If it any key in a series maps to an array, and it is the last key, the values
are output separated by ",". If key is not the final key, the following keys
are mapped to the objects that assumed to populate the array.

## Nested keys

    jsontsv 'title duplicates.Rental.HD duplicates.Rental.SD' < input.json

## Using a file to designate columns:

    jsontsv -f keys  < input.json

## Concatenating fields, truncating fields, etc.

This should be done downstream using a tool like AWK.

