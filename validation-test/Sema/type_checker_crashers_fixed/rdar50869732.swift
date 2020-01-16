// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype T : P
}

struct Generic<T> {
  init(_ value: T) {}
}

@_functionBuilder
struct Builder {
  static func buildBlock<C0, C1>(_ c0: C0, _ c1: C1) // expected-note 2 {{where 'C0' = 'Empty'}} expected-note {{where 'C1' = 'Test<Generic<(Empty, _)>>'}}
  // expected-note@-1 {{'buildBlock' declared here}}
           -> Generic<(C0, C1)> where C0 : P, C1 : P {
    return Generic((c0, c1))
  }
}

struct G<C> {
  init(@Builder _: () -> C) {}
}

struct Empty {
  init() {}
}

struct Test<T> where T : P { // expected-note {{where 'T' = 'Generic<(Empty, _)>'}}
  init(@Builder _: () -> T) {}
}

let x = G {
  // expected-error@-1 {{static method 'buildBlock' requires that 'Empty' conform to 'P'}}
  // expected-error@-2 {{static method 'buildBlock' requires that 'Test<Generic<(Empty, _)>>' conform to 'P'}}
  Empty()
  Test { Empty() }
  // expected-error@-1 {{static method 'buildBlock' requires that 'Empty' conform to 'P'}}
  // expected-error@-2 {{missing argument for parameter #2 in call}}
  // expected-error@-3 {{generic struct 'Test' requires that 'Generic<(Empty, _)>' conform to 'P'}}
}
