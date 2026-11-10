// rdar://188930383
// RUN: %target-typecheck-verify-swift

struct Key<Value> { let name: String; let defaultValue: Value }
struct A { var x: Key<Int> { .init(name: "x", defaultValue: 0) } }

@dynamicMemberLookup
struct Context {
  subscript<Value>(dynamicMember key: KeyPath<A, Key<Value>>) -> Value {
    A()[keyPath: key].defaultValue
  }
}

let c = Context()
_ = c.x
_ = c.z // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
let v = c.z // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
