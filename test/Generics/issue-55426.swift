// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/55426

protocol VectorIndex : CaseIterable {}

protocol Vector {
  associatedtype Element
  associatedtype Index: VectorIndex
  subscript(index: Index) -> Element { get set }
  init(_ indexToElementMapping: (Index) -> Element)
}

// CHECK: ExtensionDecl line={{.*}} base=Vector
// CHECK-NEXT: Generic signature: <Self where Self : Vector, Self.[Vector]Element : Vector, Self.[Vector]Index == Self.[Vector]Element.[Vector]Index, Self.[Vector]Element.[Vector]Element : BinaryFloatingPoint>

// Some methods for square matrices:
extension Vector where
  Self.Element: Vector,
  Self.Index == Self.Element.Index,
  Self.Element.Element: BinaryFloatingPoint
{
  func row(_ index: Index) -> Element { self[index] }

  func column(_ index: Index) -> Element { Element { self[$0][index] } }
}
