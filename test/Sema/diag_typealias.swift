// RUN: %target-typecheck-verify-swift

struct S {}

typealias S = S // expected-error {{type alias 'S' references itself}}
// expected-note@-1{{type declared here}}
