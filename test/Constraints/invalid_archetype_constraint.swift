// RUN: %target-typecheck-verify-swift

protocol Empty {}

protocol P {
  associatedtype Element
  init()
}

struct A<T> : P {
  typealias Element = T
}

struct A1<T> : P {
  typealias Element = T
}

struct A2<T> : P {
  typealias Element = T
}

func toA<S: Empty, AT:P>(_ s: S) -> AT where AT.Element == S.Generator.Element { // expected-error{{'Generator' is not a member type of 'S'}}
  return AT()
}
