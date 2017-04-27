// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir -num-threads 8 -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s

import resilient_struct

public protocol Runcible {
  func runce()
}

// CHECK-LABEL: @"\01l_protocol_conformances" = private constant [

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK:           [[RUNCIBLE:%swift.protocol\* @_T028protocol_conformance_records8RuncibleMp]]
// -- type metadata
// CHECK:           @_T028protocol_conformance_records15NativeValueTypeVMf
// -- witness table
// CHECK:           @_T028protocol_conformance_records15NativeValueTypeVAA8RuncibleAAWP
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
// CHECK:           @_T028protocol_conformance_records15NativeClassTypeCMf
// -- witness table
// CHECK:           @_T028protocol_conformance_records15NativeClassTypeCAA8RuncibleAAWP
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
// CHECK:           @_T028protocol_conformance_records17NativeGenericTypeVMn
// -- witness table
// CHECK:           @_T028protocol_conformance_records17NativeGenericTypeVyxGAA8RuncibleAAlWP
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
// CHECK:           @got._T0SiN
// -- witness table
// CHECK:           @_T0Si28protocol_conformance_records8RuncibleAAWP
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
// CHECK:           @got._T016resilient_struct4SizeVN
// -- witness table
// CHECK:           @_T028protocol_conformance_records17NativeGenericTypeVyxGAA8RuncibleAAlWP
// -- flags 0x04: unique direct metadata
// CHECK:           i32 1
// CHECK:         }
// CHECK:       ]

extension Size: Runcible {
  public func runce() {}
}

// TODO: conformances that need lazy initialization
