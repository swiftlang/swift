// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -enable-sil-ownership -enable-resilience %s | %FileCheck %s

public class C : Hashable {
  public static func ==(lhs: C, rhs: C) -> Bool { return lhs === rhs }
  public func hash(into: inout Hasher) {}
}

public class Base {
  public var int: Int? { fatalError() }
  public var ref: C? { fatalError()  }

  public subscript(x x: Int) -> Int? { fatalError() }
  public subscript(y y: C) -> C? { fatalError() }
}

public class Derived : Base {
  public override var int: Int { fatalError() }
  public override var ref: C { fatalError() }

  public override subscript(x x: Int?) -> Int { fatalError() }
  public override subscript(y y: C?) -> C { fatalError() }
}

public class Generic<T> {
  public var generic: T { fatalError() }

  public subscript(x x: T) -> T? { fatalError() }
  public subscript(y y: T?) -> T? { fatalError() }
}

public class DerivedGeneric<T> : Generic<T?> {
  public override var generic: T? { fatalError() }

  public subscript(x x: T) -> T { fatalError() }
  public subscript(y y: T) -> T { fatalError() }
}

public class DerivedConcrete : Generic<Int?> {
  public override var generic: Int? { fatalError() }

  public subscript(x x: Int?) -> Int { fatalError() }
  public subscript(y y: Int?) -> Int? { fatalError() }
}

@inlinable public func keyPaths() {
  _ = \Derived.int
  _ = \Derived.ref
  _ = \Derived.[x: nil]
  _ = \Derived.[y: nil]

  _ = \DerivedGeneric<Int>.generic
  _ = \DerivedGeneric<Int>.[x: nil]
  _ = \DerivedGeneric<Int>.[y: nil]

  _ = \DerivedConcrete.generic
  _ = \DerivedConcrete.[x: nil]
  _ = \DerivedConcrete.[y: nil]
}

// CHECK:      sil_property #C.hashValue ()
// CHECK-NEXT: sil_property #Base.int ()
// CHECK-NEXT: sil_property #Base.ref ()
// CHECK-NEXT: sil_property #Base.subscript ()
// CHECK-NEXT: sil_property #Base.subscript ()
// CHECK-NEXT: sil_property #Derived.int ()
// CHECK-NEXT: sil_property #Derived.subscript ()
// CHECK-NEXT: sil_property #Generic.generic<τ_0_0> ()
// CHECK-NEXT: sil_property #Generic.subscript<τ_0_0> ()
// CHECK-NEXT: sil_property #Generic.subscript<τ_0_0> ()
// CHECK-NEXT: sil_property #DerivedGeneric.subscript<τ_0_0> ()
// CHECK-NEXT: sil_property #DerivedGeneric.subscript<τ_0_0> ()
// CHECK-NEXT: sil_property #DerivedConcrete.subscript ()
// CHECK-NEXT: sil_property #DerivedConcrete.subscript ()
