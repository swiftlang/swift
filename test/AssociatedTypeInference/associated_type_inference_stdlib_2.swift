// RUN: %target-typecheck-verify-swift

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
