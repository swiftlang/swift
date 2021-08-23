// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | not %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

// XFAIL: asserts

// CHECK: Requirement signature: <Self where Self.Field : FieldAlgebra>
public protocol VectorSpace {
    associatedtype Field: FieldAlgebra
}

// CHECK: Requirement signature: <Self where Self : VectorSpace, Self == Self.Field, Self.ComparableSubalgebra : ComparableFieldAlgebra>
public protocol FieldAlgebra: VectorSpace where Self.Field == Self {
    associatedtype ComparableSubalgebra: ComparableFieldAlgebra
    static var zero: Self { get }
}

// CHECK: Requirement signature: <Self where Self : FieldAlgebra, Self == Self.ComparableSubalgebra>
public protocol ComparableFieldAlgebra: FieldAlgebra where Self.ComparableSubalgebra == Self {
}

// CHECK: Generic signature: <F where F : FieldAlgebra>
public func test<F: FieldAlgebra>(_ f: F) {
    _ = F.ComparableSubalgebra.zero
}
