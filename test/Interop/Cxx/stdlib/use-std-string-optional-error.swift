// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default
import CxxStdlib

let tmpOpt: String? = "üüüüüüü"
let cppString1 = std.string(tmpOpt) // expected-warning {{'init(_:)' is deprecated: unwrap the optional value and use init(_ string: UnsafePointer<CChar>) instead}}

let tmpNonOpt: String = "üüüüüüü"
let cppString2 = std.string(tmpNonOpt)
