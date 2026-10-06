// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import StdHash
import CxxStdlib

let hash = D.hash(into:) // expected-error {{type 'D' has no member 'hash(into:)'}}

let dictC: [D : String] = [:] // expected-error {{type 'D' does not conform to protocol 'Hashable'}}

let dictD: [E : String] = [:] // expected-error {{type 'E' does not conform to protocol 'Hashable'}}

let dictF: [F : String] = [:] // expected-error {{type 'F' does not conform to protocol 'Hashable'}}

let dictG: [G : String] = [:] // expected-error {{type 'G' does not conform to protocol 'Hashable'}}
