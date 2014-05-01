// RUN: %swift -parse %s -verify

import NonExistent // expected-error {{no such module}}

// No subsequent diagnostics after fatal errors.
var x: NonExistent.Something
