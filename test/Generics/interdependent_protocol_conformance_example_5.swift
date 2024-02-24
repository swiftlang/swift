// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -disable-requirement-machine-reuse 2>&1 | %FileCheck %s

// CHECK-LABEL: .NonEmptyProtocol@
// CHECK-NEXT: Requirement signature: <Self where Self : Collection, Self.[NonEmptyProtocol]C : Collection, Self.[Sequence]Element == Self.[NonEmptyProtocol]C.[Sequence]Element, Self.[Collection]Index == Self.[NonEmptyProtocol]C.[Collection]Index>

public protocol NonEmptyProtocol: Collection
  where Element == C.Element,
        Index == C.Index {
  associatedtype C: Collection
}

// CHECK-LABEL: .MultiPoint@
// CHECK-NEXT: Requirement signature: <Self where Self.[MultiPoint]C : CoordinateSystem, Self.[MultiPoint]P == Self.[MultiPoint]C.[CoordinateSystem]P, Self.[MultiPoint]X : NonEmptyProtocol, Self.[MultiPoint]C.[CoordinateSystem]P == Self.[MultiPoint]X.[Sequence]Element, Self.[MultiPoint]X.[NonEmptyProtocol]C : NonEmptyProtocol>

public protocol MultiPoint {
  associatedtype C: CoordinateSystem
  associatedtype P: Point where Self.P == Self.C.P

  associatedtype X: NonEmptyProtocol
    where X.C: NonEmptyProtocol,
          X.Element == Self.P 
}

// CHECK-LABEL: .CoordinateSystem@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[CoordinateSystem]B.[BoundingBox]C, Self.[CoordinateSystem]B : BoundingBox, Self.[CoordinateSystem]L : Line, Self.[CoordinateSystem]P : Point, Self.[CoordinateSystem]S : Size, Self.[CoordinateSystem]B.[BoundingBox]C == Self.[CoordinateSystem]L.[MultiPoint]C, Self.[CoordinateSystem]L.[MultiPoint]C == Self.[CoordinateSystem]S.[Size]C>

public protocol CoordinateSystem {
  associatedtype P: Point where Self.P.C == Self
  associatedtype S: Size where Self.S.C == Self
  associatedtype L: Line where Self.L.C == Self
  associatedtype B: BoundingBox where Self.B.C == Self
}

// CHECK-LABEL: .Line@
// CHECK-NEXT: Requirement signature: <Self where Self : MultiPoint, Self.[MultiPoint]C == Self.[MultiPoint]P.[Point]C>

public protocol Line: MultiPoint {}

// CHECK-LABEL: .Size@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Size]C.[CoordinateSystem]S, Self.[Size]C : CoordinateSystem>

public protocol Size {
  associatedtype C: CoordinateSystem where Self.C.S == Self
}

// CHECK-LABEL: .BoundingBox@
// CHECK-NEXT: Requirement signature: <Self where Self.[BoundingBox]C : CoordinateSystem>

public protocol BoundingBox {
  associatedtype C: CoordinateSystem
  typealias P = Self.C.P
  typealias S = Self.C.S
}

// CHECK-LABEL: .Point@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[Point]C.[CoordinateSystem]P, Self.[Point]C : CoordinateSystem>

public protocol Point { 
  associatedtype C: CoordinateSystem where Self.C.P == Self
}

func sameType<T>(_: T, _: T) {}

func conformsToPoint<T : Point>(_: T.Type) {}

func testMultiPoint<T : MultiPoint>(_: T) {
  sameType(T.C.P.self, T.X.Element.self)
  conformsToPoint(T.P.self)
}

func testCoordinateSystem<T : CoordinateSystem>(_: T) {
  sameType(T.P.C.self, T.self)
}
