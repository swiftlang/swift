// RUN: %target-typecheck-verify-swift

typealias A = B // expected-error {{type alias 'A' references itself}} expected-note {{while resolving type 'B'}}
typealias C = D // expected-note {{through reference here}} expected-note {{while resolving type 'D'}}
typealias D = (A, Int) // expected-note {{through reference here}} expected-note {{while resolving type '(A, Int)'}}
typealias B = C // expected-note {{through reference here}} expected-note {{while resolving type 'C'}}
