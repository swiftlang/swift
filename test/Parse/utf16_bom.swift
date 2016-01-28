// RUN: %target-parse-verify-swift
ÿþ // expected-error{{input files must be encoded as UTF-8 instead of UTF-16}}
asdfadf() // no error, the BOM marker causes us to skip to the end
