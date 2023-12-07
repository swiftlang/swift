// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference
// RUN: not %target-typecheck-verify-swift -disable-experimental-associated-type-inference

protocol IP {
  associatedtype E

  func next() -> E?
}

protocol S {
  associatedtype I: IP
  associatedtype E where E == I.E

  func makeI() -> I
}

struct G: S {
  struct I: IP {
    func next() -> Int? {}
  }

  func makeI() -> I {}
}
