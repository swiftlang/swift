// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default
import CxxStdlib

let tmp: String? = "üüüüüüü"
let cppString = std.string(tmp) // expected-error {{'init(_:)' is unavailable: Passing Optional String to the initializer of std::string is not supported. Please unwrap the value before passing to std.string()}}
