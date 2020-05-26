// RUN: %target-typecheck-verify-swift

struct S {}

typealias S = S // expected-error {{type alias 'S' references itself}} expected-note {{while resolving type 'S'}} expected-note {{through reference here}}
