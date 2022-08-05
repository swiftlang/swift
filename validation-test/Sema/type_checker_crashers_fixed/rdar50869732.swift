// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype T : P
}

struct Generic<T> {
  init(_ value: T) {}
}

@resultBuilder
struct Builder {
  static func buildBlock<C0, C1>(_ c0: C0, _ c1: C1) // expected-note {{where 'C0' = 'Empty'}}
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

struct Test<T> where T : P {
  init(@Builder _: () -> T) {}
}

let x = G {
  Empty()
  Test { Empty() }
  // expected-error@-1 {{static method 'buildBlock' requires that 'Empty' conform to 'P'}}
  // expected-error@-2 {{missing argument for parameter #2 in call}}
}
