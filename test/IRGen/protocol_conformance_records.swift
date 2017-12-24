// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -num-threads 8 -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s

import resilient_struct

public protocol Runcible {
  func runce()
}

// CHECK-LABEL: @"\01l_protocol_conformances" = private constant [

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE:@_T028protocol_conformance_records8RuncibleMp]]
// -- type metadata
// CHECK-SAME:           @_T028protocol_conformance_records15NativeValueTypeVMf
// -- witness table
// CHECK-SAME:           @_T028protocol_conformance_records15NativeValueTypeVAA8RuncibleAAWP
// -- flags 0x01: unique direct metadata
// CHECK-SAME:           i32 1
// CHECK-SAME:         },
public struct NativeValueType: Runcible {
  public func runce() {}
}

// -- TODO class refs should be indirected through their ref variable
// CHECK-SAME:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- class object (TODO should be class ref variable)
// CHECK-SAME:           @_T028protocol_conformance_records15NativeClassTypeCMf
// -- witness table
// CHECK-SAME:           @_T028protocol_conformance_records15NativeClassTypeCAA8RuncibleAAWP
// -- flags 0x01: unique direct metadata (TODO should be 0x03 indirect class)
// CHECK-SAME:           i32 1
// CHECK-SAME:         },
public class NativeClassType: Runcible {
  public func runce() {}
}

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- nominal type descriptor
// CHECK-SAME:           @_T028protocol_conformance_records17NativeGenericTypeVMn
// -- witness table
// CHECK-SAME:           @_T028protocol_conformance_records17NativeGenericTypeVyxGAA8RuncibleAAWP
// -- flags 0x04: unique nominal type descriptor
// CHECK-SAME:           i32 4
// CHECK-SAME:         },
public struct NativeGenericType<T>: Runcible {
  public func runce() {}
}

// CHECK-SAME:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- type metadata
// CHECK-SAME:           @got._T0SiN
// -- witness table
// CHECK-SAME:           @_T0Si28protocol_conformance_records8RuncibleAAWP
// -- flags 0x01: unique direct metadata
// CHECK-SAME:           i32 1
// CHECK-SAME:         }
extension Int: Runcible {
  public func runce() {}
}

// For a resilient struct, reference the NominalTypeDescriptor

// CHECK-SAME:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- nominal type descriptor
// CHECK-SAME:           @got._T016resilient_struct4SizeVN
// -- witness table
// CHECK-SAME:           @_T016resilient_struct4SizeV28protocol_conformance_records8RuncibleADWP
// -- flags 0x04: unique direct metadata
// CHECK-SAME:           i32 1
// CHECK-SAME:         }

extension Size: Runcible {
  public func runce() {}
}

// TODO: conformances that need lazy initialization
public protocol Spoon { }

// Conditional conformances
// CHECK: %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[SPOON:@_T028protocol_conformance_records5SpoonMp]]
// -- nominal type descriptor
// CHECK-SAME:           @_T028protocol_conformance_records17NativeGenericTypeVMn
// -- witness table accessor
// CHECK-SAME:           @_T028protocol_conformance_records17NativeGenericTypeVyxGAA5SpoonA2aERzlWa
// -- flags 0x04: unique nominal type descriptor + conditional accessor
// CHECK-SAME:           i32 36
// CHECK-SAME:         }
extension NativeGenericType : Spoon where T: Spoon {
  public func runce() {}
}
