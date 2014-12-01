# jsontsv

A simple tool to transform JSON into tab-separated line-oriented output
amenable to Unix text processing. 

Input should be a stream of JSON objects with mostly uniform keys.

Output keys must be specified. If none are specified, all the top-level keys of
the first object are taken as the template.


  jsontsv 
