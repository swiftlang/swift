// RUN: %target-typecheck-verify-swift

// Parsing and placement of the '@unsafe(always)' attribute.

@unsafe(always) func alwaysUnsafe() { }

@unsafe(bogus) func unknownOption() { }
// expected-error@-1{{unknown option 'bogus' for attribute 'unsafe'}}

@unsafe() func missingOption() { }
// expected-error@-1{{expected 'unsafe' option such as 'always'}}
// expected-error@-2{{expected declaration}}

@safe @unsafe(always) func conflicting() { }
// expected-error@-1{{global function 'conflicting' cannot be both '@safe' and '@unsafe'}}

@unsafe(always) typealias AlwaysUnsafeAlias = Int

@unsafe(always) var alwaysUnsafeGlobal: Int = 0
