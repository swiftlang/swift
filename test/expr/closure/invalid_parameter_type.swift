// RUN: %target-parse-verify-swift

var f = { (s: Undeclared)-> Int in 0 } // expected-error {{use of undeclared type 'Undeclared'}}
