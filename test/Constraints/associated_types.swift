// RUN: %target-typecheck-verify-swift

protocol Runcible {
  associatedtype Runcee
}

class Mince {
  init() {}
}

class Spoon : Runcible {
  init() {}

  typealias Runcee = Mince
}

class Owl<T:Runcible> {
  init() {}

  func eat(_ what: T.Runcee, with: T) { }
}

func owl1() -> Owl<Spoon> {
  return Owl<Spoon>()
}

func owl2() -> Owl<Spoon> {
  return Owl()
}

func owl3() {
  Owl<Spoon>().eat(Mince(), with:Spoon())
}

// "Can't access associated types through class-constrained generic parameters"
// (https://bugs.swift.org/browse/SR-726)
func spoon<S: Spoon>(_ s: S) {
  let _: S.Runcee?
}
