// RUN: %target-swift-emit-sil %s -parse-as-library -wmo -module-name test -enable-experimental-feature Embedded | %FileCheck %s
// RUN: %target-swift-emit-sil %s -parse-as-library -wmo -module-name test -enable-experimental-feature Embedded -DNESTED | %FileCheck %s

// REQUIRES: swift_feature_Embedded

// rdar://189228012
//
// The witnesses of `[UInt8]: P` are not generic, because the conformance's only
// generic parameter is bound to a concrete type. Specializing the witness table
// of the specialized conformance `[UInt8]: P` used to crash, both when it was
// used directly and when it was an associated conformance.

public protocol P {
  var bytes: Int { get }
}

extension Array: P where Element == UInt8 {
  public var bytes: Int { count }
}

#if NESTED

public protocol Q {
  associatedtype B: P
}

public struct S: Q {
  public typealias B = [UInt8]
}

public func test() -> any Q { S() }

#else

public func test() -> any P { [UInt8]() }

#endif

// CHECK-LABEL: sil_witness_table shared [specialized] Array<UInt8>: specialize <UInt8> (<Element where Element == UInt8> Array<Element>: P module test) {
// CHECK-NEXT:    method #P.bytes!getter: {{.*}} : @$eSays5UInt8VG4test1PA2dEP5bytesSivgTW
// CHECK-NEXT:  }
