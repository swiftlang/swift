// RUN: %target-typecheck-verify-swift -parse-stdlib -disable-availability-checking

import Swift

class Klass {}

// Make sure we ignore this attribute and error in swift mode.

let l: @moveOnly Klass = Klass() // expected-error {{unknown attribute 'moveOnly'}}
