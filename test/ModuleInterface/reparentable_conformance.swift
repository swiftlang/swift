// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name conformances -enable-experimental-feature Reparenting
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name conformances -enable-experimental-feature Reparenting
// RUN: %FileCheck %s < %t.swiftinterface

public protocol Thingable {}

public struct DefaultThingable: Thingable {}

@available(macOS 75, *)
@reparentable
public protocol MiniBorrowSeq<Element> {
  associatedtype Element

  associatedtype BorrowIter: Thingable = DefaultThingable
}

// CHECK-LABEL: public struct Adapter<Base> : conformances.MiniBorrowSeq where Base : conformances.MiniBorrowSeq {
@available(macOS 75, *)
public struct Adapter<Base: MiniBorrowSeq>: MiniBorrowSeq {
  // CHECK-NEXT: public typealias Element = Base.Element
  public typealias Element = Base.Element
  // CHECK-NEXT: @available(macOS 75, *)
  // CHECK-NEXT: @_implements(conformances.MiniBorrowSeq, BorrowIter) public typealias __MiniBorrowSeq_BorrowIter = conformances.DefaultThingable
}

public struct Outer<Element, BorrowIter> where BorrowIter: Thingable {
  // CHECK-LABEL: public struct Inner : conformances.MiniBorrowSeq {
  public struct Inner: MiniBorrowSeq {
  // CHECK-NEXT: @_implements(conformances.MiniBorrowSeq, BorrowIter) public typealias __MiniBorrowSeq_BorrowIter = BorrowIter
  // CHECK-NEXT: @_implements(conformances.MiniBorrowSeq, Element) public typealias __MiniBorrowSeq_Element = Element
  }
}

public struct UMBP<Element> {}

// CHECK-LABEL: extension conformances.UMBP : conformances.MiniBorrowSeq {
extension UMBP: MiniBorrowSeq {
// CHECK-NEXT: @_implements(conformances.MiniBorrowSeq, BorrowIter) public typealias __MiniBorrowSeq_BorrowIter = conformances.DefaultThingable
// CHECK-NEXT: @_implements(conformances.MiniBorrowSeq, Element) public typealias __MiniBorrowSeq_Element = Element

  @available(macOS 75, *)
  func whatever() {}
}

extension UMBP where Self.Element: Sendable {}
