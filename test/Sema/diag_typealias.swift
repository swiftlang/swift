// RUN: %target-typecheck-verify-swift

struct S {}

typealias S = S // expected-error {{redundant type alias declaration}}{{1-17=}}

