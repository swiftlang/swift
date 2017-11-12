// RUN: not %target-swift-frontend %s -typecheck

// Bug with associated type inference. Really, this should type check
// with 'SomeAssociatedType' inferred as 'Any', but for now, it's good
// enough to not crash.

protocol SomeProtocol {
  associatedtype SomeAssociatedType = Any
  static func staticFunc(_ arg: SomeAssociatedType)
  func memberFunc(_ arg: SomeAssociatedType)
}

class SomeClass: SomeProtocol {
  func memberFunc(_ arg: SomeClass.SomeAssociatedType) {}
  static func staticFunc(_ arg: SomeClass.SomeAssociatedType) {}
}
