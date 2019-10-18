// RUN: %target-typecheck-verify-swift

typealias A = B // expected-error {{type alias 'A' references itself}} expected-note {{through reference here}}
typealias C = D // expected-note {{through reference here}} expected-note {{through reference here}}
typealias D = (A, Int) // expected-note {{through reference here}} expected-note {{through reference here}}
typealias B = C // expected-note {{through reference here}} expected-note {{through reference here}}
