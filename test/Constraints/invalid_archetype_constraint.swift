// RUN: %swift -parse %s -verify

protocol P {
  typealias Element
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

func toA<S: _Sequence, AT:P where AT.Element == S.GeneratorType.Element>(s: S) -> AT { // expected-error{{'GeneratorType' is not a member type of 'S'}}
  return AT()
}
