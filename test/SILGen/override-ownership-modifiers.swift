// RUN: %target-swift-emit-silgen %s

// This is the valid subset of test/Sema/override-ownership-modifiers.swift

class Base {
  init(noExplicitOwnership: String) {}
  init(consume: consuming String) {}
  init(borrow: borrowing String) {}

  func noExplicitOwnership(arg: String) {}
  func borrow(arg: borrowing String) {}
  func consume(arg: consuming String) {}
}

class ExplicitImplicit: Base {
  // OK to override parameters with the same ownership spelled explicitly.
  override init(noExplicitOwnership: consuming String) { }
  override init(consume: String) { }

  override func noExplicitOwnership(arg: borrowing String) {}
  override func borrow(arg: String) {}
}
