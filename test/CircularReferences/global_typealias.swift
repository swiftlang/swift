// RUN: %target-typecheck-verify-swift

typealias A = B // expected-note{{type declared here}}
typealias C = D
typealias D = (A, Int) // expected-error{{type alias 'A' references itself}}
typealias B = C
