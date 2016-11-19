// RUN: %target-typecheck-verify-swift

// Warn about non-trailing closures followed by parameters with
// default arguments.
func f1(_ f: () -> (), bar: Int = 10) { } // expected-warning{{closure parameter prior to parameters with default arguments will not be treated as a trailing closure}}
func f2(_ f: (() -> ())!, bar: Int = 10, wibble: Int = 17) { } // expected-warning{{closure parameter prior to parameters with default arguments will not be treated as a trailing closure}}
func f3(_ f: (() -> ())?, bar: Int = 10) { } // expected-warning{{closure parameter prior to parameters with default arguments will not be treated as a trailing closure}}

// An extra set of parentheses suppresses the warning.
func g1(_ f: (() -> ()), bar: Int = 10) { } // no-warning

// Stop at the first closure.
func g2(_ f: () -> (), g: (() -> ())? = nil) { } // no-warning
