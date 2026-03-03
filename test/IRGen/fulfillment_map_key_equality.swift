// RUN: %target-swift-frontend -emit-ir %s -enable-library-evolution | %FileCheck %s

// rdar://160649141

public protocol P1 {}

public protocol P2 {
  associatedtype A1
}

public protocol P5 {
  associatedtype A2: P2
}

public protocol P3 where A4.A1 == A3.A2.A1 {
  associatedtype A3: P5
  associatedtype A4: P2

  var x: Int32 { get }
}

public protocol P6: P3 {}

public protocol P4 {
  associatedtype A4: P2
}

public struct G1<A1>: P2 {}

public struct G2<A2: P2>: P5  {}

public struct G3<T: P6 & P4>: P3 where T.A4.A1: P1 {
  public typealias A4 = G1<T.A3.A2.A1>
  public typealias A3 = G2<T.A3.A2>

  // Make sure this witness thunk doesn't have any additional bogus parameters.

  // CHECK-LABEL: define internal swiftcc i32 @"$s28fulfillment_map_key_equality2G3VyxGAA2P3A2aEP1xs5Int32VvgTW"(ptr noalias swiftself captures(none) %0, ptr %Self, ptr %SelfWitnessTable) {{.*}} {
  public var x: Int32 { fatalError() }
}
