// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -verify-ignore-unrelated

import StdFunction

let _ = FunctionTooManyParams({ _, _, _, _, _, _, _ in return 123 }) // expected-error {{'init(_:)' is unavailable: function takes too many parameters}}
