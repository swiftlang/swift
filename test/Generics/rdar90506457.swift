// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]B : P2, Self.[P1]C : P4, Self.[P1]D : P5, Self.[P1]E : P6, Self.[P1]F : P3, Self.[P1]G == Self.[P1]B.[P2]G, Self.[P1]B.[P2]G == Self.[P1]C.[P4]G, Self.[P1]C.[P4]G == Self.[P1]D.[P5]G, Self.[P1]D.[P5]G == Self.[P1]E.[P6]G, Self.[P1]F.[Sequence]Element == SIMD2<Self.[P1]G>>
public protocol P1 {
  associatedtype G
  associatedtype B: P2 where B.G == G
  associatedtype C: P4 where C.G == G
  associatedtype D: P5 where D.G == G
  associatedtype E: P6 where E.G == G
  associatedtype F: P3 where F.Element == SIMD2<G>
}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]A : P1, Self.[P2]C : P4, Self.[P2]D : P5, Self.[P2]E : P6, Self.[P2]G == Self.[P2]A.[P1]G, Self.[P2]A.[P1]G == Self.[P2]C.[P4]G, Self.[P2]C.[P4]G == Self.[P2]D.[P5]G, Self.[P2]D.[P5]G == Self.[P2]E.[P6]G>
public protocol P2 {
  associatedtype G
  associatedtype A: P1 where A.G == G
  associatedtype C: P4 where C.G == G
  associatedtype D: P5 where D.G == G
  associatedtype E: P6 where E.G == G
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self : MutableCollection, Self : RandomAccessCollection, Self.[Collection]Index == Int>
public protocol P3: RandomAccessCollection, MutableCollection where Index == Int {}

// CHECK-LABEL: .P4@
// CHECK-NEXT: Requirement signature: <Self where Self.[P4]G : SIMDScalar>
public protocol P4 {
  associatedtype G: SIMDScalar
}

// CHECK-LABEL: .P5@
// CHECK-NEXT: Requirement signature: <Self where Self.[P5]G : SIMDScalar>
public protocol P5 {
  associatedtype G: SIMDScalar
}

// CHECK-LABEL: .P6@
// CHECK-NEXT: Requirement signature: <Self where Self.[P6]G : SIMDScalar>
public protocol P6 {
  associatedtype G: SIMDScalar
}
