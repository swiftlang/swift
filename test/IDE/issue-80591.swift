// RUN: %batch-code-completion -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence

infix operator ^^^

extension Optional where Wrapped: ~Escapable & ~Copyable {
  @lifetime(copy self) mutating func foo() -> Self { fatalError() }
}

func ^^^ <T: ~Escapable & ~Copyable> (_ x: Int, _ y: borrowing T?) {}

// https://github.com/swiftlang/swift/issues/80591 - Make sure we don't crash
// here.
func foo() {
  _ = 1 ^^^ .#^COMPLETE^#
  // COMPLETE: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: foo({#(self): &Optional<~Copyable & ~Escapable>#})[#() -> Optional<~Copyable & ~Escapable>#]; name=foo(:)
}
