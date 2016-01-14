// RUN: %target-parse-verify-swift

protocol P {
  typealias Assoc = Self
}

struct X : P {
}

class Y<T: P> {
  typealias Assoc = T.Assoc
}

func f<T: P>(x: T, y: Y<T>.Assoc) {
}

protocol P1 {
  typealias A = Int
}

struct X1<T> : P1 {
  init(_: X1.A) {
  }
}
