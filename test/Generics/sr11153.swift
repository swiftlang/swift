// RUN: %target-swift-frontend -typecheck -debug-generic-signatures %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/apple/swift/issues/53550

// CHECK: Requirement signature: <Self where Self.[VectorSpace]Field : FieldAlgebra>
public protocol VectorSpace {
    associatedtype Field: FieldAlgebra
}

// CHECK: Requirement signature: <Self where Self : VectorSpace, Self == Self.[VectorSpace]Field, Self.[FieldAlgebra]ComparableSubalgebra : ComparableFieldAlgebra>
public protocol FieldAlgebra: VectorSpace where Self.Field == Self {
    associatedtype ComparableSubalgebra: ComparableFieldAlgebra
    static var zero: Self { get }
}

// CHECK: Requirement signature: <Self where Self : FieldAlgebra, Self == Self.[FieldAlgebra]ComparableSubalgebra>
public protocol ComparableFieldAlgebra: FieldAlgebra where Self.ComparableSubalgebra == Self {
}

// CHECK: Generic signature: <F where F : FieldAlgebra>
public func test<F: FieldAlgebra>(_ f: F) {
    _ = F.ComparableSubalgebra.zero
}
