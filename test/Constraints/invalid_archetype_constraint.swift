// RUN: %target-parse-verify-swift

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

func toA<S: _SequenceType, AT:P where AT.Element == S.Generator.Element>(s: S) -> AT { // expected-error{{'Generator' is not a member type of 'S'}}
  return AT()
}
