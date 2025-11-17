// RUN: %target-typecheck-verify-swift %s -I %S/Inputs -cxx-interoperability-mode=default -diagnostic-style llvm

import StdHash
import CxxStdlib

let hash = C.hash(into:) // expected-error {{type 'C' has no member 'hash(into:)'}}

let dictC: [C : String] = [:] // expected-error {{type 'C' does not conform to protocol 'Hashable'}}

let dictD: [D : String] = [:] // expected-error {{type 'D' does not conform to protocol 'Hashable'}}
