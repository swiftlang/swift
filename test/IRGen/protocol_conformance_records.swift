// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -num-threads 8 -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s

import resilient_struct
import resilient_protocol

public protocol Runcible {
  func runce()
}

// CHECK-LABEL: @"\01l_protocols" = private constant [

// CHECK: %swift.protocolref {
// CHECK-SAME: @"$S28protocol_conformance_records8RuncibleMp"
// CHECK-SAME: %swift.protocolref {
// CHECK-SAME: @"$S28protocol_conformance_records5SpoonMp"

// CHECK-LABEL: @"\01l_protocol_conformances" = private constant [

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE:@"\$S28protocol_conformance_records8RuncibleMp"]]
// -- type metadata
// CHECK-SAME:           @"$S28protocol_conformance_records15NativeValueTypeVMn"
// -- witness table
// CHECK-SAME:           @"$S28protocol_conformance_records15NativeValueTypeVAA8RuncibleAAWP"
// -- flags
// CHECK-SAME:           i32 0
// CHECK-SAME:         },
public struct NativeValueType: Runcible {
  public func runce() {}
}

// CHECK-SAME:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- class metadata
// CHECK-SAME:           @"$S28protocol_conformance_records15NativeClassTypeCMn"
// -- witness table
// CHECK-SAME:           @"$S28protocol_conformance_records15NativeClassTypeCAA8RuncibleAAWP"
// -- flags
// CHECK-SAME:           i32 0
// CHECK-SAME:         },
public class NativeClassType: Runcible {
  public func runce() {}
}

// CHECK:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- nominal type descriptor
// CHECK-SAME:           @"$S28protocol_conformance_records17NativeGenericTypeVMn"
// -- witness table
// CHECK-SAME:           @"$S28protocol_conformance_records17NativeGenericTypeVyxGAA8RuncibleAAWP"
// -- flags
// CHECK-SAME:           i32 0
// CHECK-SAME:         },
public struct NativeGenericType<T>: Runcible {
  public func runce() {}
}

// CHECK-SAME:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- type metadata
// CHECK-SAME:           @"got.$SSiMn"
// -- witness table
// CHECK-SAME:           @"$SSi28protocol_conformance_records8RuncibleAAWP"
// -- reserved
// CHECK-SAME:           i32 8
// CHECK-SAME:         }
extension Int: Runcible {
  public func runce() {}
}

// For a resilient struct, reference the NominalTypeDescriptor

// CHECK-SAME:         %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- nominal type descriptor
// CHECK-SAME:           @"got.$S16resilient_struct4SizeVMn"
// -- witness table
// CHECK-SAME:           @"$S16resilient_struct4SizeV28protocol_conformance_records8RuncibleADWP"
// -- reserved
// CHECK-SAME:           i32 8
// CHECK-SAME:         }

extension Size: Runcible {
  public func runce() {}
}

// TODO: conformances that need lazy initialization
public protocol Spoon { }

// Conditional conformances
// CHECK: %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           [[SPOON:@"\$S28protocol_conformance_records5SpoonMp"]]
// -- nominal type descriptor
// CHECK-SAME:           @"$S28protocol_conformance_records17NativeGenericTypeVMn"
// -- witness table accessor
// CHECK-SAME:           @"$S28protocol_conformance_records17NativeGenericTypeVyxGAA5SpoonA2aERzlWa
// -- flags
// CHECK-SAME:           i32 258
// CHECK-SAME:         }
extension NativeGenericType : Spoon where T: Spoon {
  public func runce() {}
}

// Retroactive conformance
// CHECK: %swift.protocol_conformance {
// -- protocol descriptor
// CHECK-SAME:           @"got.$S18resilient_protocol22OtherResilientProtocolMp"
// -- nominal type descriptor
// CHECK-SAME:           @"got.$SSiMn"
// -- witness table accessor
// CHECK-SAME:           @"$SSi18resilient_protocol22OtherResilientProtocol0B20_conformance_recordsWa"
// -- flags
// CHECK-SAME:           i32 73
// CHECK-SAME:         }
extension Int : OtherResilientProtocol { }
