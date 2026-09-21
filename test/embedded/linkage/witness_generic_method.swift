// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature Embedded -wmo -emit-ir -o - | %FileCheck %s

// REQUIRES: swift_feature_Embedded

// In Embedded Swift, a protocol requirement that takes an unspecialized
// non-class-bound generic parameter cannot be dispatched through a witness
// table (the witness would have to be an unspecialized generic function,
// which Embedded does not support). When such a conformance gets a strongly
// emitted witness table (via @export(interface) or CodeGenerationModel=interface),
// IRGen used to assert. Now the unsupported slot is filled with the
// `swift_deletedMethodError` stub so the rest of the witness table emits.

public protocol P {
  associatedtype A
  static func convert<Q: P>(_ q: Q.A, from: Q.Type) -> A?
}

@export(interface)
public struct S: P {
  public typealias A = Int
  public static func convert<Q: P>(_ q: Q.A, from: Q.Type) -> Int? {
    return nil
  }
}

// The conformance witness table is emitted, and the `convert` slot points at
// the deleted-method error stub.
//
// CHECK: @"$e{{[0-9]+}}{{[A-Za-z0-9_]*}}1SVAA1PAAWP" = {{.*}}global {{.*}}@swift_deletedMethodError
