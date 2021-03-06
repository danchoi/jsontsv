# jsontsv

Transforms JSON objects into delimiter-separated line-oriented output, which is
more convenient for downstream processing with Unix tools like `grep`, `awk`,
`diff`, etc. Also useful for converting JSON data into spreadsheet data: CSV-style
output is supported.

## Synopsis

input:

```json
{
  "title": "Terminator 2: Judgment Day",
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

    jsontsv 'title year stars.name ratings.imdb' < input

outputs this tab-separated text:

```tsv
Terminator 2: Judgment Day	1991	Arnold Schwarzenegger,Linda Hamilton	8.5
Interstellar	2014	Matthew McConaughey,Anne Hathaway	8.9
```

You can pick off array elements using `[i]` syntax:

    jsontsv 'title year stars[0].name' < input

outputs 

```tsv
Terminator 2: Judgment Day     1991    Arnold Schwarzenegger
Interstellar    2014    Matthew McConaughey
```

Use the `-H` flag to output the headers and the Unix tool `column` to align the
columns for prettier output:

    cat input | jsontsv -H 'title year stars.name ratings.imdb' | column -s $'\t' -t

outputs

```
title                       year  stars.name                            ratings.imdb
Terminator 2: Judgment Day  1991  Arnold Schwarzenegger,Linda Hamilton  8.5
Interstellar                2014  Matthew McConaughey,Anne Hathaway     8.9
```

## Mapping an array index getter over nested arrays

As of version 0.1.5.0, you can map an index getter over arrays, such as this

    {"id":2,"cast":[["Michael Caine",13473],["Demi Moore",65231]]}

with this expression

    jsontsv -a '|' 'id cast._[0]' < input

This results in:
    
    2   Michael Caine|Demi Moore

## Extracting 2-tuple values

From version 0.1.5.0, you can can access lists of pairs, e.g.

    {"menu":[["dinner","fish"],["dessert","pie"]]}

with the expression `["KEY"]`, e.g.:

    jsontsv 'menu["dinner"]'
    # => fish

    jsontsv 'menu["dessert"]'
    # => pie

    jsontsv 'menu["drink"]'
    # => null

This is useful because Data.Aeson emits Haskell 2-tuples in this format:

    ghci> encode [("dinner", "fish"),("dessert","pie")]
    "[[\"dinner\",\"fish\"],[\"dessert\",\"pie\"]]"

Moreover, encoding 2-tuples is often preferable to encoding `Data.Map.Map` because 
encoding 2-tuples preserves ordering, whereas encoding `Map` does not.

## Installation

Assuming you have a recent version of the [Haskell
platform](https://www.haskell.org/platform/) on your system, 

    cabal update
    cabal install jsontsv

Alternatively, 

    git clone git@github.com:danchoi/jsontsv.git
    cd jsontsv
    cabal sandbox init
    cabal install 
    # Now copy .cabal-sandbox/bin/jsontsv to your PATH

## Usage

```
jsontsv

Usage: jsontsv FIELDS [-a DELIM] ([-c|--csv] | [-d DELIM]) [-H]
               [-n|--null-string STRING] [-t|--true-string STRING]
               [-f|--false-string STRING] [-N|--newline STRING] [--debug]
  Transform JSON objects to TSV. On STDIN provide an input stream of
  whitespace-separated JSON objects.

Available options:
  -h,--help                Show this help text
  -a DELIM                 Concatentated array elem delimiter. Defaults to
                           comma.
  -c,--csv                 Output CSV
  -d DELIM                 Output field delimiter. Defaults to tab.
  -H                       Include headers
  -n,--null-string STRING  String to represent null value. Default: 'null'
  -t,--true-string STRING  String to represent boolean true. Default: 't'
  -f,--false-string STRING String to represent boolean false. Default: 'f'
  -N,--newline STRING      String to replace newlines in field text. Default: '
                           '
  --debug                  Debug keypaths

See https://github.com/danchoi/jsontsv for more information.
```

Input should be a stream of JSON objects of the same or mostly similar shape,
separated by whitespace such as newlines. If the objects are wrapped in a JSON
array at the top level or nested inside a top-level object, use the [jq][jq]
tool by Stephan Dolan to extract an object stream, e.g.: 

[jq]:http://stedolan.github.io/jq/

    curl -s "https://api.github.com/repos/rails/rails/issues" | 
    jq -M '.[]' | 
    jsontsv -H 'number title user.login state repository.name labels.name' 

outputs

```
number	title	user.login	state	repository.name	labels.name
17894	Add default value for `create_table_definition`	kamipo	open	null	
17893	Vendor/assets/images not being precompiled	runephilosof	open	null	
17891	Removed use of mocha in the info_controller tests	prathamesh-sonpatki	open	null	
17887	Wrong instance object passed to lambda on has_many :through	haruska	open	null	
17885	Update postgresql_database_tasks.rb	starbelly	open	null	
17884	Routes with {trailing_slash: true} do not match if referenced as non-named routes	dreyks	open	null	
17880	Fix humanize for already upcased acronyms	mintuhouse	open	null	activesupport
17879	humanize doesn't respect Infector acronyms	mintuhouse	open	null	activesupport
17864	eager loading a has_many through association ignores order of through association	jturkel	open	null	
17859	Includes HABTM returns correct size now	scambra	open	null	
17858	test preloading a HABTM association with hash conditions	scambra	open	null	
17854	Bug when using find_in_batches and reverse_order	robertjlooby	open	null	activerecord
17853	Remove deprecated `reset_changes` and `reset_attribute!` methods.	kaspth	open	null	
17851	Support for any type primary key	kamipo	open	null	
17845	Don't leak Object constants in core_ext/module/qualified_const	gsamokovarov	open	null	
17825	Fix Sidekiq ActiveJob integration setup	aripollak	open	null	activejob
17824	AR::RecordNotSaved & RecordNotDestroyed from save!/destroy! should include an error message	yuki24	open	null	
17822	Refactor `visit_ChangeColumnDefinition`	kamipo	open	null	
17820	Clear query cache on rollback	fw42	open	null	
17819	handle_positional_args does not work properly in route with format: false option	vevisystems	open	null	actionpack
17817	Hide potentially sensitive ActiveJob params from logs	aripollak	open	null	activejob
17815	Remove custom errors page section from the guides	yuki24	open	null	
17813	Changed welcome#index page overall look and feel	wazery	open	null	railties
17804	Null values will still be passed to custom serializers.	xaviershay	open	null	activerecord
17797	Don't remove mailer layouts files	y-yagi	open	null	
17795	ActiveRecord joins/includes bug	dgobaud	open	null	activerecord,regression
17793	Fix undesirable RangeError by Type::Integer. Add Type::UnsignedInteger.	kamipo	open	null	activerecord
17792	allow 'all' for :domain option in addition to :all	rockrep	open	null	
17788	Issue#17703 Test case for tempfile attribute	sivagollapalli	open	null	
17787	rails runner does not respect subdirectory / how to specify subdirectory?	doits	open	null	railties
```

JSON leaf values are printed as follows: 

* Strings and numbers are copied to output.
* Boolean values are output as `t` or `f`. You can changes this with the -t and -f options.
* null is printed as `null`. You can change this with the -n option.
* If the leaf value is an array, it is concatenated into a single
  comma-separated string. This delimiter can be changed with the `-a` option.

## Newlines in content

If a string field in the JSON contains a `\n` or `\r` character, these will be replaced 
by spaces by default. The replacement character can be changed with the `-N` option.

## Column header aliases

If the default column headers using `-H` are too long, you can designate
aliases with the pattern `[keypath]:[alias]`. E.g., 

    curl -s "https://api.github.com/repos/rails/rails/issues" | 
    jq -M '.[]' | 
    jsontsv -H 'number title user.login:login state repository.name:repo_name' 


## Known alternatives 

* [jsawk](https://github.com/micha/jsawk) Jsawk is like awk, but for JSON. (nodejs)
* [json2csv](https://github.com/jehiah/json2csv) made with Go
* [jq][jq] a lightweight and flexible command-line JSON processor
* [jsoncsv](https://github.com/gradus/jsoncsv) a json to csv library in javascript/coffeescript
* [json](https://github.com/konklone/json) A free, in-browser JSON to CSV converter
* [tsvutils](https://github.com/brendano/tsvutils) utilities for processing tab-separated files

## Related

* [table](https://github.com/danchoi/table)
* [jsonsql](https://github.com/danchoi/jsonsql)
* [tsvsql](https://github.com/danchoi/tsvsql)
* [jsonxlsx](https://github.com/danchoi/jsonxlsx)

## Further reading

* [Seven command line tools for data
  science](http://jeroenjanssens.com/2013/09/19/seven-command-line-tools-for-data-science.html) by Jeroen Janssens.
  A blog post surveying tools like this.
* [Data Science at the Command Line: Facing the Future with Time-Tested Tools](http://shop.oreilly.com/product/0636920032823.do)
  by Jeroen Janssens. A book published in September 2014 by O'Reilly.
* [jsontsv versus json2csv](https://github.com/jeroenjanssens/data-science-at-the-command-line/issues/26) 
