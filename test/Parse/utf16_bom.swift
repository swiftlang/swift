// RUN: %target-typecheck-verify-swift
ÿþ // expected-cparser-error{{input files must be encoded as UTF-8 instead of UTF-16}} expected-astgen-error{{invalid UTF-8 found in source file}}
asdfadf() // no error, the BOM marker causes us to skip to the end expected-astgen-error {{cannot find 'asdfadf' in scope}}
