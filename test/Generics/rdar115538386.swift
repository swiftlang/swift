// RUN: %target-swift-frontend -typecheck %s -disable-availability-checking

protocol P<A> {
  associatedtype A
}

struct S<each T>: P {
  typealias A = (repeat each T)
}

func foo<each T>() -> some P<(repeat each T)> {
  S<repeat each T>()
}
