// RUN: %target-typecheck-verify-swift

protocol P {}
struct A<C> {}
extension A: P where A: P {} // expected-error {{requirement involves recursion that is not currently supported}}
