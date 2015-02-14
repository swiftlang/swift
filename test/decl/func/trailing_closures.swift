// RUN: %target-parse-verify-swift

// Warn about non-trailing closures followed by parameters with
// default arguments.
func f1(f: () -> (), bar: Int = 10) { } // expected-warning{{closure parameter prior to parameters with default arguments will not be treated as a trailing closure}}
func f2(f: (() -> ())!, bar: Int = 10, wibble: Int = 17) { } // expected-warning{{closure parameter prior to parameters with default arguments will not be treated as a trailing closure}}
func f3(f: (() -> ())?, bar: Int = 10) { } // expected-warning{{closure parameter prior to parameters with default arguments will not be treated as a trailing closure}}
func f4(var f: (() -> ())?, bar: Int = 10) { } // expected-warning{{closure parameter prior to parameters with default arguments will not be treated as a trailing closure}}

// An extra set of parentheses suppresses the warning.
func g1(f: (() -> ()), bar: Int = 10) { } // no-warning

// Stop at the first closure.
func g2(f: () -> (), g: (() -> ())? = nil) { } // no-warning
