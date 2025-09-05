// RUN: %target-typecheck-verify-swift -dynamic-member-lookup-depth-limit=2

@dynamicMemberLookup
struct Lens<T> {
  init() {}
  subscript<U>(dynamicMember kp: KeyPath<T, U>) -> U {
    fatalError()
  }
}

struct S {
  var x: Int
}

_ = \Lens<S>.x // Fine
_ = \Lens<Lens<S>>.x // Also fine
_ = \Lens<Lens<Lens<S>>>.x // expected-error {{could not find member 'x'; exceeded the maximum number of nested dynamic member lookups}}

_ = Lens<S>().x // Fine
_ = Lens<Lens<S>>().x // Also fine
_ = Lens<Lens<Lens<S>>>().x // expected-error {{could not find member 'x'; exceeded the maximum number of nested dynamic member lookups}}
