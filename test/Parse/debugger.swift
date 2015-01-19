// RUN: %target-parse-verify-swift -debugger-support

import Nonexistent_Module // expected-error {{no such module}}

var ($x0, $x1) = (4, 3)
var z = $x0 + $x1

z // no error.

var x: Double = z // expected-error {{'Int' is not convertible to 'Double'}}
