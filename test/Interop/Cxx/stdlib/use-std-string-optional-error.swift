// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default
import CxxStdlib

let tmpOpt: String? = "üüüüüüü"
let cppString1 = std.string(tmpOpt) // expected-warning {{'init(_:)' is deprecated: Replaced by public init(_ string: UnsafePointer<CChar>) which takes a non-optional argument.}}

let tmpNonOpt: String = "üüüüüüü"
let cppString2 = std.string(tmpNonOpt)
