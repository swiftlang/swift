// RUN: %target-swift-frontend -typecheck -verify %s

enum Foo<T> {
  indirect case A // expected-error{{enum case 'A' without associated value cannot be 'indirect'}}
  indirect case B(T)
  indirect case C, D(T) // expected-error{{enum case 'C' without associated value cannot be 'indirect'}}
}

indirect enum Barbie<T> {
  case A, B(T)
}

indirect enum Bar<T> {
  case A
  indirect case B(T) // expected-error{{enum case in 'indirect' enum cannot also be 'indirect'}}
}

indirect struct Bas { // expected-error{{cannot be applied}} {{1-10=}}
  indirect var x: Int // expected-error{{cannot be applied}} {{3-12=}}
}
