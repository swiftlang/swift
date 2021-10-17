// RUN: %target-typecheck-verify-swift -parse-stdlib -disable-availability-checking -verify-syntax-tree

import Swift

class Klass {}

// Make sure we ignore this attribute and error in swift mode.

let l: @moveOnly Klass = Klass() // expected-error {{attribute does not apply to type}}
