// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype T : P
}

struct Generic<T> {
  init(_ value: T) {}
}

@_functionBuilder
struct Builder {
  static func buildBlock<C0, C1>(_ c0: C0, _ c1: C1)
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
  Test { <#code#> } // expected-error {{editor placeholder in source file}}
}
