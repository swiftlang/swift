// RUN: %target-typecheck-verify-swift -swift-version 4

// ----------------------------------------------------------------------------
// Semi-bogus factory init pattern -- Swift 4 permitted this for non-final
// classes, which is unsound, but swift-corelibs-foundation made use of
// this, so possibly there are other usages in the wild.
//
// See test/decl/func/dynamic_self.swift for the Swift 5 test case.

protocol FactoryPattern {
  init(factory: @autoclosure () -> Self)
}

extension  FactoryPattern {
  init(factory: @autoclosure () -> Self) { self = factory() }
}

class Factory : FactoryPattern {
  init(_string: String) {}

  convenience init(string: String) {
    self.init(factory: Factory(_string: string))
  }
}
