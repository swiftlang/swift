// RUN: %target-parse-verify-swift

protocol P {
  associatedtype Assoc = Self
}

struct X : P {
}

class Y<T: P> {
  typealias Assoc = T.Assoc
}

func f<T: P>(_ x: T, y: Y<T>.Assoc) {
}

protocol P1 {
  associatedtype A = Int
}

struct X1<T> : P1 {
  init(_: X1.A) {
  }
}
