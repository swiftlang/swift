// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir -num-threads 8 -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s

import resilient_struct

public protocol Runcible {
  func runce()
}

// CHECK-LABEL: @"\01l_protocol_conformances" = private constant [

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK:           [[RUNCIBLE:%swift.protocol\* @_TMp28protocol_conformance_records8Runcible]]
// -- type metadata
// CHECK:           @_TMfV28protocol_conformance_records15NativeValueType
// -- witness table
// CHECK:           @_TWPV28protocol_conformance_records15NativeValueTypeS_8Runcible
// -- flags 0x01: unique direct metadata
// CHECK:           i32 1
// CHECK:         },
public struct NativeValueType: Runcible {
  public func runce() {}
}

// -- TODO class refs should be indirected through their ref variable
// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK:           [[RUNCIBLE]]
// -- class object (TODO should be class ref variable)
// CHECK:           @_TMfC28protocol_conformance_records15NativeClassType
// -- witness table
// CHECK:           @_TWPC28protocol_conformance_records15NativeClassTypeS_8Runcible
// -- flags 0x01: unique direct metadata (TODO should be 0x03 indirect class)
// CHECK:           i32 1
// CHECK:         },
public class NativeClassType: Runcible {
  public func runce() {}
}

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK:           [[RUNCIBLE]]
// -- nominal type descriptor
// CHECK:           @_TMnV28protocol_conformance_records17NativeGenericType
// -- witness table
// CHECK:           @_TWPurGV28protocol_conformance_records17NativeGenericTypex_S_8RuncibleS_
// -- flags 0x04: unique nominal type descriptor
// CHECK:           i32 4
// CHECK:         },
public struct NativeGenericType<T>: Runcible {
  public func runce() {}
}

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK:           [[RUNCIBLE]]
// -- type metadata
// CHECK:           @got._TMSi
// -- witness table
// CHECK:           @_TWPSi28protocol_conformance_records8Runcible
// -- flags 0x01: unique direct metadata
// CHECK:           i32 1
// CHECK:         }
extension Int: Runcible {
  public func runce() {}
}

// For a resilient struct, reference the NominalTypeDescriptor

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK:           [[RUNCIBLE]]
// -- nominal type descriptor
// CHECK:           @got._TMV16resilient_struct4Size
// -- witness table
// CHECK:           @_TWPurGV28protocol_conformance_records17NativeGenericTypex_S_8RuncibleS_
// -- flags 0x04: unique direct metadata
// CHECK:           i32 1
// CHECK:         }
// CHECK:       ]

extension Size: Runcible {
  public func runce() {}
}

// TODO: conformances that need lazy initialization
