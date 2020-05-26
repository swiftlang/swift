public protocol P1 {
  associatedtype A1: SomeClass
}

public protocol P4: P2 where A2: P1 {}


public class SomeClass { }

protocol P5 {
  associatedtype A3: P4
}

struct Foo {
  static func f<T: P5>(_: T) {
  }
}
