// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default
import CxxStdlib

let tmpOpt: String? = "üüüüüüü"
let cppString1 = std.string(tmpOpt) // expected-warning {{'init(_:)' is deprecated: Passing an optional C string pointer is discouraged. Use the non-nullable overload or wrap in a String first.}}

let tmpNonOpt: String = "üüüüüüü"
let cppString2 = std.string(tmpNonOpt)