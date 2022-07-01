// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: .Tree@
// CHECK-NEXT: Requirement signature: <Self where Self.[Tree]Distance : FloatingPoint, Self.[Tree]Distance : SIMDScalar, Self.[Tree]Distance == Self.[Tree]Point.[SIMDStorage]Scalar, Self.[Tree]Point : SIMD>
protocol Tree {
  associatedtype Distance: FloatingPoint & SIMDScalar
  associatedtype Point: SIMD where Point.Scalar == Distance
}

// CHECK-LABEL: .QuadTree@
// CHECK-NEXT: Requirement signature: <Self where Self : Tree, Self.[Tree]Point == SIMD2<Self.[Tree]Distance>>
protocol QuadTree : Tree where Point == SIMD2<Distance> {}

// CHECK-LABEL: .OctTree@
// CHECK-NEXT: Requirement signature: <Self where Self : Tree, Self.[Tree]Point == SIMD3<Self.[Tree]Distance>>
protocol OctTree : Tree where Point == SIMD3<Distance> {}

func sameType<T>(_: T.Type, _: T.Type) {}

extension QuadTree {
  func foo() {
    sameType(Point.MaskStorage.self, SIMD2<Distance.SIMDMaskScalar>.self)
    sameType(Point.MaskStorage.MaskStorage.self, SIMD2<Distance.SIMDMaskScalar>.self)
  }
}

extension OctTree {
  func foo() {
    sameType(Point.MaskStorage.self, SIMD3<Distance.SIMDMaskScalar>.self)
    sameType(Point.MaskStorage.MaskStorage.self, SIMD3<Distance.SIMDMaskScalar>.self)
  }
}
