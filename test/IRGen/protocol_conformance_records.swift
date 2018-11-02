// RUN: %target-swift-frontend -primary-file %s -emit-ir -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -num-threads 8 -enable-resilience -enable-source-import -I %S/../Inputs | %FileCheck %s

import resilient_struct
import resilient_protocol

public protocol Associate {
  associatedtype X
}

// Dependent conformance
// CHECK-LABEL: @"$S28protocol_conformance_records9DependentVyxGAA9AssociateAAMc" ={{ dllexport | protected | }}constant
// -- protocol descriptor
// CHECK-SAME:           @"$S28protocol_conformance_records9AssociateMp"
// -- nominal type descriptor
// CHECK-SAME:           @"$S28protocol_conformance_records9DependentVMn"
// -- witness table accessor
// CHECK-SAME:           @"$S28protocol_conformance_records9DependentVyxGAA9AssociateAAWa"
// -- flags
// CHECK-SAME:           i32 1
// CHECK-SAME:         }
public struct Dependent<T> {}

extension Dependent : Associate {
  public typealias X = (T, T)
}

public protocol Runcible {
  func runce()
}

// CHECK-LABEL: @"$S28protocol_conformance_records15NativeValueTypeVAA8RuncibleAAMc" ={{ dllexport | protected | }}constant %swift.protocol_conformance_descriptor {
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

// CHECK-LABEL:         @"$S28protocol_conformance_records15NativeClassTypeCAA8RuncibleAAMc" ={{ dllexport | protected | }}constant %swift.protocol_conformance_descriptor {
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

// CHECK-LABEL:         @"$S28protocol_conformance_records17NativeGenericTypeVyxGAA8RuncibleAAMc" ={{ dllexport | protected | }}constant %swift.protocol_conformance_descriptor {
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

// CHECK-LABEL:         @"$SSi28protocol_conformance_records8RuncibleAAMc" ={{ dllexport | protected | }}constant %swift.protocol_conformance_descriptor {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- type metadata
// CHECK-SAME:           @"{{got.|__imp_}}$SSiMn"
// -- witness table
// CHECK-SAME:           @"$SSi28protocol_conformance_records8RuncibleAAWP"
// -- reserved
// CHECK-SAME:           i32 8
// CHECK-SAME:         }
extension Int: Runcible {
  public func runce() {}
}

// For a resilient struct, reference the NominalTypeDescriptor

// CHECK-LABEL:         @"$S16resilient_struct4SizeV28protocol_conformance_records8RuncibleADMc" ={{ dllexport | protected | }}constant %swift.protocol_conformance_descriptor {
// -- protocol descriptor
// CHECK-SAME:           [[RUNCIBLE]]
// -- nominal type descriptor
// CHECK-SAME:           @"{{got.|__imp_}}$S16resilient_struct4SizeVMn"
// -- witness table
// CHECK-SAME:           @"$S16resilient_struct4SizeV28protocol_conformance_records8RuncibleADWP"
// -- reserved
// CHECK-SAME:           i32 8
// CHECK-SAME:         }

extension Size: Runcible {
  public func runce() {}
}

// CHECK-LABEL: @"\01l_protocols"
// CHECK-SAME: @"$S28protocol_conformance_records8RuncibleMp"
// CHECK-SAME: @"$S28protocol_conformance_records5SpoonMp"

public protocol Spoon { }

// Conditional conformances
// CHECK-LABEL: {{^}}@"$S28protocol_conformance_records17NativeGenericTypeVyxGAA5SpoonA2aERzlMc" ={{ dllexport | protected | }}constant
// -- protocol descriptor
// CHECK-SAME:           @"$S28protocol_conformance_records5SpoonMp"
// -- nominal type descriptor
// CHECK-SAME:           @"$S28protocol_conformance_records17NativeGenericTypeVMn"
// -- witness table accessor
// CHECK-SAME:           @"$S28protocol_conformance_records17NativeGenericTypeVyxGAA5SpoonA2aERzlWa
// -- flags
// CHECK-SAME:           i32 258
// -- conditional requirement #1
// CHECK-SAME:           i32 128,
// CHECK-SAME:           i32 0,
// CHECK-SAME:           @"$S28protocol_conformance_records5SpoonMp"
// CHECK-SAME:         }
extension NativeGenericType : Spoon where T: Spoon {
  public func runce() {}
}

// Retroactive conformance
// CHECK-LABEL: @"$SSi18resilient_protocol22OtherResilientProtocol0B20_conformance_recordsMc" ={{ dllexport | protected | }}constant
// -- protocol descriptor
// CHECK-SAME:           @"{{got.|__imp_}}$S18resilient_protocol22OtherResilientProtocolMp"
// -- nominal type descriptor
// CHECK-SAME:           @"{{got.|__imp_}}$SSiMn"
// -- witness table accessor
// CHECK-SAME:           @"$SSi18resilient_protocol22OtherResilientProtocol0B20_conformance_recordsWa"
// -- flags
// CHECK-SAME:           i32 73,
// -- module context for retroactive conformance
// CHECK-SAME:           @"$S28protocol_conformance_recordsMXM"
// CHECK-SAME:         }
extension Int : OtherResilientProtocol { }

// CHECK-LABEL: @"\01l_protocol_conformances" = private constant
// CHECK-SAME: @"$S28protocol_conformance_records15NativeValueTypeVAA8RuncibleAAMc"
// CHECK-SAME: @"$S28protocol_conformance_records15NativeClassTypeCAA8RuncibleAAMc"
// CHECK-SAME: @"$S28protocol_conformance_records17NativeGenericTypeVyxGAA8RuncibleAAMc"
// CHECK-SAME: @"$S16resilient_struct4SizeV28protocol_conformance_records8RuncibleADMc"
// CHECK-SAME: @"$S28protocol_conformance_records17NativeGenericTypeVyxGAA5SpoonA2aERzlMc"
// CHECK-SAME: @"$SSi18resilient_protocol22OtherResilientProtocol0B20_conformance_recordsMc"
