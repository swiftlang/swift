// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/pre_specialized_module_layouts.swiftmodule %S/Inputs/pre_specialized_module_layouts.swift
// RUN: %target-swift-frontend -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s --check-prefix=OPT -check-prefix=OPT-%target-os
// RUN: %target-swift-frontend -I %t -Onone -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s --check-prefix=NONE -check-prefix=NONE-%target-os


// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -emit-module-path %t/pre_specialized_module_layouts.swiftmodule %S/Inputs/pre_specialized_module_layouts.swift
// RUN: %target-swift-frontend -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s --check-prefix=OPT

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -enable-library-evolution -emit-module-path %t/pre_specialized_module_layouts.swiftmodule %S/Inputs/pre_specialized_module_layouts.swift
// RUN: %target-swift-frontend -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s --check-prefix=OPT

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -swift-version 5 -enable-library-evolution -emit-module -o /dev/null -emit-module-interface-path %t/pre_specialized_module_layouts.swiftinterface %S/Inputs/pre_specialized_module_layouts.swift -module-name pre_specialized_module_layouts
// RUN: %target-swift-frontend -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s --check-prefix=OPT

import pre_specialized_module_layouts

// Helper to prevent return values from getting optimized away
@inline(never)
public func consume<T>(_ x: T) {}

public struct ReferenceWrapperStruct {
  let x: AnyObject
}

@_alignment(16)
public struct OveralignedReferenceWrapperStruct {
  let x: AnyObject
}

public struct TwoInt32 {
  let x: Int32 = 0
  let y: Int32 = 0
}

public struct Stride96 {
  let x: Int32 = 0
  let y: Int32 = 0
  let z: Bool = false
}

// Make sure we generate the public pre-specialized entry points.

// OPT-DAG: sil @$s22pre_specialize_layouts10testPublic1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// OPT-DAG: sil @$s22pre_specialize_layouts10testPublic1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// OPT-DAG: sil @$s22pre_specialize_layouts10testPublic1tyx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> () {
// OPT-macosx-DAG: sil [available 10.5] @$s22pre_specialize_layouts10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-linux-gnu-DAG: sil @$s22pre_specialize_layouts10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {

// NONE-DAG: sil @$s22pre_specialize_layouts10testPublic1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// NONE-DAG: sil @$s22pre_specialize_layouts10testPublic1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// NONE-DAG: sil @$s22pre_specialize_layouts10testPublic1tyx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> () {
// NONE-macosx-DAG: sil [available 10.5] @$s22pre_specialize_layouts10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// NONE-linux-gnu-DAG: sil @$s22pre_specialize_layouts10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {

@_specialize(exported: true, where T == Int)
@_specialize(exported: true, where T == Float)
@_specialize(exported: true, where @_noMetadata T : _Class)
@_specialize(exported: true, availability: macOS 10.5, *; where T == Double)
public func testPublic<T>(t: T) {
  print(t)
}

// OPT-macosx-DAG: sil [available 10.5] @$s22pre_specialize_layouts18testEmitIntoClient1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-linux-gnu-DAG: sil @$s22pre_specialize_layouts18testEmitIntoClient1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-DAG: sil @$s22pre_specialize_layouts18testEmitIntoClient1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// OPT-DAG: sil @$s22pre_specialize_layouts18testEmitIntoClient1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// OPT-DAG: sil @$s22pre_specialize_layouts18testEmitIntoClient1tyx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> () {

// NONE-DAG: sil @$s22pre_specialize_layouts18testEmitIntoClient1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// NONE-DAG: sil @$s22pre_specialize_layouts18testEmitIntoClient1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// NONE-DAG: sil @$s22pre_specialize_layouts18testEmitIntoClient1tyx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> () {

@_specialize(exported: true, where T == Int)
@_specialize(exported: true, where T == Float)
@_specialize(exported: true, where @_noMetadata T : _Class)
@_specialize(exported: true, where @_noMetadata T : _BridgeObject)
@_specialize(exported: true, availability: macOS 10.5, *; where T == Double)
@_alwaysEmitIntoClient
internal func testEmitIntoClient<T>(t: T) {
  print(t)
}

// OPT: sil @$s22pre_specialize_layouts28usePrespecializedEntryPoints13wrapperStruct11overaligned5array8stride96yAA016ReferenceWrapperI0V_AA011OveralignedmnI0VSaySiGAA8Stride96VtF : $@convention(thin) (@guaranteed ReferenceWrapperStruct, @guaranteed OveralignedReferenceWrapperStruct, @guaranteed Array<Int>, Stride96) -> () {
// OPT: bb0([[P1:%.*]] : $ReferenceWrapperStruct, [[P2:%.*]] : $OveralignedReferenceWrapperStruct, [[P3:%.*]] : $Array<Int>, [[P4:%.*]] : $Stride96):
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFSi_Ts5 : $@convention(thin) (Int) -> ()
// OPT:   apply [[F1]]
// OPT:   [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFSd_Ts5 : $@convention(thin) (Double) -> ()
// OPT:   apply [[F2]]
// OPT:   [[F9:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFBi64__Ts5 : $@convention(thin) (Builtin.Int64) -> ()
// OPT:   [[A5:%.*]] = unchecked_trivial_bit_cast {{%.*}} : $UInt64 to $Builtin.Int64
// OPT:   apply [[F9]]([[A5]]) : $@convention(thin) (Builtin.Int64) -> ()
// OPT:   [[A6:%.*]] = unchecked_trivial_bit_cast {{%.*}} : $TwoInt32 to $Builtin.Int64
// OPT:   apply [[F9]]([[A6]]) : $@convention(thin) (Builtin.Int64) -> ()
// OPT-macosx:   [[F6:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// OPT-macosx:    apply [[F6]]<SomeData>
// OPT: [[F7:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT: [[A1:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT: apply [[F7]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT: [[A2:%.*]] = unchecked_bitwise_cast [[P1]] : $ReferenceWrapperStruct to $AnyObject
// OPT: apply [[F7]]([[A2]]) : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT: [[A3:%.*]] = alloc_stack $OveralignedReferenceWrapperStruct
// OPT: apply {{%.*}}<OveralignedReferenceWrapperStruct>([[A3]])
// OPT-macosx: [[F8:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFBb_Ts5 : $@convention(thin) (@guaranteed Builtin.BridgeObject) -> ()
// OPT-macosx: [[A4:%.*]] = unchecked_bitwise_cast [[P3]] : $Array<Int> to $Builtin.BridgeObject
// OPT-macosx: apply [[F8]]([[A4]]) : $@convention(thin) (@guaranteed Builtin.BridgeObject) -> ()
// OPT:   [[F10:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFBi8_Bv12__Ts5 : $@convention(thin) (Builtin.Vec12xInt8) -> ()
// OPT:   [[A6:%.*]] = unchecked_trivial_bit_cast [[P4]] : $Stride96 to $Builtin.Vec12xInt8
// OPT:   apply [[F10]]([[A6]]) : $@convention(thin) (Builtin.Vec12xInt8) -> ()
// OPT:   [[F3:%.*]] = function_ref @$s30pre_specialized_module_layouts36internalEmitIntoClientPrespecializedyyxlFSi_Ts5 : $@convention(thin) (Int) -> ()
// OPT:   apply [[F3]]
// OPT:   [[F4:%.*]] = function_ref @$s30pre_specialized_module_layouts36internalEmitIntoClientPrespecializedyyxlFSd_Ts5 : $@convention(thin) (Double) -> ()
// OPT:   apply [[F4]]
// OPT:   [[F5:%.*]] = function_ref @$s30pre_specialized_module_layouts16useInternalThingyyxlFSi_Tg5
// OPT:   apply [[F5]]({{.*}}) : $@convention(thin) (Int) -> ()
// OPT: } // end sil function '$s22pre_specialize_layouts28usePrespecializedEntryPoints13wrapperStruct11overaligned5array8stride96yAA016ReferenceWrapperI0V_AA011OveralignedmnI0VSaySiGAA8Stride96VtF'

// OPT: sil {{.*}} @$s30pre_specialized_module_layouts16useInternalThingyyxlFSi_Tg5 : $@convention(thin) (Int) -> () {
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V7computexyFSi_Ts5 : $@convention(method) (InternalThing2<Int>) -> Int
// OPT:   apply [[F1]](
// OPT:   [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedXxvgSi_Ts5 : $@convention(method) (InternalThing2<Int>) -> Int
// OPT:   apply [[F2]](
// OPT:   [[F3:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedYxvsSi_Ts5 : $@convention(method) (Int, @inout InternalThing2<Int>) -> ()
// OPT:   apply [[F3]](
// OPT:   [[F4:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedYxvgSi_Ts5 : $@convention(method) (InternalThing2<Int>) -> Int
// OPT:   apply [[F4]](
// OPT:   [[F5:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedZxvMSi_Ts5 : $@yield_once @convention(method) (@inout InternalThing2<Int>) -> @yields @inout Int
// OPT:   begin_apply [[F5]](
// OPT:   [[F6:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedZxvrSi_Ts5 : $@yield_once @convention(method) (InternalThing2<Int>) -> @yields @in_guaranteed Int
// OPT:   begin_apply [[F6]](
// OPT:   [[F7:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2VyxSicisSi_Ts5 : $@convention(method) (Int, Int, @inout InternalThing2<Int>) -> ()
// OPT:   apply [[F7]](
// OPT:   [[F8:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2VyxSicigSi_Ts5 : $@convention(method) (Int, InternalThing2<Int>) -> Int
// OPT:   apply [[F8]](
// OPT: } // end sil function '$s30pre_specialized_module_layouts16useInternalThingyyxlFSi_Tg5'

// OPT: sil shared @$s30pre_specialized_module_layouts16useInternalThingyyxlFAA9SomeClassC_Tg5 : $@convention(thin) (@guaranteed SomeClass) -> () {
// OPT:   [[R1:%.*]] = init_existential_addr {{%.*}} : $*Any, $SomeClass
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V7computexyFyXl_Ts5 : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[R2:%.*]] = unchecked_addr_cast [[R1]] : $*SomeClass to $*AnyObject
// OPT:   [[R3:%.*]] = apply [[F1]]({{.*}}) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   store [[R3]] to [[R2]] : $*AnyObject
// OPT:   [[R4:%.*]] = init_existential_addr {{%.*}} : $*Any, $SomeClass
// OPT:   [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedXxvgyXl_Ts5 : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[R5:%.*]] = unchecked_addr_cast [[R4]] : $*SomeClass to $*AnyObject
// OPT:   [[R6:%.*]] = apply [[F2]]({{.*}}) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   store [[R6]] to [[R5]] : $*AnyObject
// OPT:   [[F3:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedYxvsyXl_Ts5 : $@convention(method) (@owned AnyObject, @inout InternalThing2<AnyObject>) -> ()
// OPT:   [[A3:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT:   apply [[F3]]([[A3]], {{.*}}) : $@convention(method) (@owned AnyObject, @inout InternalThing2<AnyObject>) -> ()
// OPT:   [[F4:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedYxvgyXl_Ts5 : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[R5:%.*]] = apply [[F4]]({{.*}}) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[F5:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedZxvMyXl_Ts5 : $@yield_once @convention(method) (@inout InternalThing2<AnyObject>) -> @yields @inout AnyObject
// OPT:   ([[R7:%.*]], {{%.*}}) = begin_apply [[F5]]({{.*}}) : $@yield_once @convention(method) (@inout InternalThing2<AnyObject>) -> @yields @inout AnyObject
// OPT:   [[R8:%.*]] = unchecked_addr_cast [[R7]] : $*AnyObject to $*SomeClass
// OPT:   [[F6:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedZxvryXl_Ts5 : $@yield_once @convention(method) (@guaranteed InternalThing2<AnyObject>) -> @yields @in_guaranteed AnyObject
// OPT:   ([[R9:%.*]], {{%.*}}) = begin_apply [[F6]]({{.*}}) : $@yield_once @convention(method) (@guaranteed InternalThing2<AnyObject>) -> @yields @in_guaranteed AnyObject
// OPT:   [[R10:%.*]] = unchecked_addr_cast [[R9]] : $*AnyObject to $*SomeClass
// OPT: } // end sil function '$s30pre_specialized_module_layouts16useInternalThingyyxlFAA9SomeClassC_Tg5'

public func usePrespecializedEntryPoints(wrapperStruct: ReferenceWrapperStruct, overaligned: OveralignedReferenceWrapperStruct, array: [Int], stride96: Stride96) {
  publicPrespecialized(1)
  publicPrespecialized(1.0)
  publicPrespecialized(UInt64(1))
  publicPrespecialized(TwoInt32())
  publicPrespecialized(SomeData())
  publicPrespecialized(SomeClass())
  publicPrespecialized(wrapperStruct)
  // should not apply _Class specialization for overaligned struct
  publicPrespecialized(overaligned)
  publicPrespecialized(array)
  publicPrespecialized(stride96)
  useInternalEmitIntoClientPrespecialized(2)
  useInternalEmitIntoClientPrespecialized(2.0)
  useInternalThing(2)
  useInternalThing(SomeClass())
}

// OPT: sil @$s22pre_specialize_layouts46usePrespecializedEntryPointsWithMarkerProtocol1ty0a20_specialized_module_C09SomeClassC_tF : $@convention(thin) (@guaranteed SomeClass) -> () {
// OPT: bb0([[P1:%.*]] : $SomeClass):
// OPT:   [[R1:%.*]] = alloc_stack $SomeClass
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts38publicPrespecializedWithMarkerProtocolyxxs8SendableRzlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> @owned AnyObject
// OPT:   [[R2:%.*]] = unchecked_addr_cast [[R1]] : $*SomeClass to $*AnyObject
// OPT:   [[A1:%.*]] = unchecked_ref_cast [[P1]] : $SomeClass to $AnyObject
// OPT:   [[R3:%.*]] = apply [[F1]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> @owned AnyObject
// OPT:   store [[R3]] to [[R2]] : $*AnyObject
// OPT:   [[F2:%.*]] = function_ref @$s22pre_specialize_layouts7consumeyyxlF0a20_specialized_module_C09SomeClassC_Ttg5 : $@convention(thin) () -> ()
// OPT:   apply [[F2]]() : $@convention(thin) () -> ()
// OPT:   dealloc_stack [[R1]] : $*SomeClass
// OPT: } // end sil function '$s22pre_specialize_layouts46usePrespecializedEntryPointsWithMarkerProtocol1ty0a20_specialized_module_C09SomeClassC_tF'
public func usePrespecializedEntryPointsWithMarkerProtocol(t: SomeClass) {
  consume(publicPrespecializedWithMarkerProtocol(t))
}

// OPT: sil @$s22pre_specialize_layouts34usePrespecializedThrowsEntryPointsyyKF : $@convention(thin) () -> @error any Error {
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts26publicPrespecializedThrowsyxxKlFSi_Ts5 : $@convention(thin) (Int) -> (Int, @error any Error)
// OPT:   try_apply [[F1]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error any Error)
// OPT-macosx:   [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts26publicPrespecializedThrowsyxxKlFAA8SomeDataV_Tg5 : $@convention(thin) (SomeData) -> (SomeData, @error any Error)
// OPT-macosx:   try_apply [[F2]]({{%.*}}) : $@convention(thin) (SomeData) -> (SomeData, @error any Error)
// OPT:   [[F3:%.*]] = function_ref @$s30pre_specialized_module_layouts26publicPrespecializedThrowsyxxKlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> (@owned AnyObject, @error any Error)
// OPT:   [[A1:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT:   try_apply [[F3]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> (@owned AnyObject, @error any Error), normal [[BB1:bb.*]], error
// OPT: [[BB1]]([[A2:%.*]] : $AnyObject):
// OPT:   [[R1:%.*]] = unchecked_addr_cast {{%.*}} : $*SomeClass to $*AnyObject
// OPT:   store [[A2]] to [[R1]] : $*AnyObject
// OPT: } // end sil function '$s22pre_specialize_layouts34usePrespecializedThrowsEntryPointsyyKF'
public func usePrespecializedThrowsEntryPoints() throws {
  consume(try publicPrespecializedThrows(1))
  consume(try publicPrespecializedThrows(SomeData()))
  consume(try publicPrespecializedThrows(SomeClass()))
}

// OPT: sil @$s22pre_specialize_layouts40usePresepcializedMultipleIndirectResults___2xs2ysy0a20_specialized_module_C09SomeClassC_AA0m5OtherN0Cs5Int64VSaySiGSaySfGtF : $@convention(thin) (@guaranteed SomeClass, @guaranteed SomeOtherClass, Int64, @guaranteed Array<Int>, @guaranteed Array<Float>) -> () {
// OPT: {{bb.*}}([[P1:%.*]] : $SomeClass, [[P2:%.*]] : $SomeOtherClass, [[P3:%.*]] : $Int64, [[P4:.*]] : $Array<Int>, [[P5:%.*]] : $Array<Float>):
// OPT:   [[R1:%.*]] = alloc_stack $SomeOtherClass
// OPT:   [[R2:%.*]] = alloc_stack $SomeClass
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts43publicPresepcializedMultipleIndirectResultsyq__s5Int64Vxtx_q_ADtr0_lFyXl_yXlTs5 : $@convention(thin) (@guaranteed AnyObject, @guaranteed AnyObject, Int64) -> (@out AnyObject, Int64, @out AnyObject)
// OPT:   [[R3:%.*]] = unchecked_addr_cast [[R1]] : $*SomeOtherClass to $*AnyObject
// OPT:   [[R4:%.*]] = unchecked_addr_cast [[R2]] : $*SomeClass to $*AnyObject
// OPT:   [[A1:%.*]] = unchecked_ref_cast [[P1]] : $SomeClass to $AnyObject
// OPT:   [[A2:%.*]] = unchecked_ref_cast [[P2]] : $SomeOtherClass to $AnyObject
// OPT:   apply [[F1]]([[R3]], [[R4]], [[A1]], [[A2]], [[P3]]) : $@convention(thin) (@guaranteed AnyObject, @guaranteed AnyObject, Int64) -> (@out AnyObject, Int64, @out AnyObject)
// OPT-macosx:   [[R6:%.*]] = alloc_stack $Array<Float>
// OPT-macosx:   [[R7:%.*]] = alloc_stack $Array<Int>
// OPT-macosx:   [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts43publicPresepcializedMultipleIndirectResultsyq__s5Int64Vxtx_q_ADtr0_lFBb_BbTs5 : $@convention(thin) (@guaranteed Builtin.BridgeObject, @guaranteed Builtin.BridgeObject, Int64) -> (@out Builtin.BridgeObject, Int64, @out Builtin.BridgeObject)
// OPT-macosx:   [[R8:%.*]] = unchecked_addr_cast [[R6]] : $*Array<Float> to $*Builtin.BridgeObject
// OPT-macosx:   [[R9:%.*]] = unchecked_addr_cast [[R7]] : $*Array<Int> to $*Builtin.BridgeObject
// OPT-macosx:   [[A3:%.*]] = unchecked_bitwise_cast [[P4]] : $Array<Int> to $Builtin.BridgeObject
// OPT-macosx:   [[A4:%.*]] = unchecked_bitwise_cast [[P5]] : $Array<Float> to $Builtin.BridgeObject
// OPT-macosx:   [[F2]]([[R8]], [[R9]], [[A3]], [[A4]], [[P3]]) : $@convention(thin) (@guaranteed Builtin.BridgeObject, @guaranteed Builtin.BridgeObject, Int64) -> (@out Builtin.BridgeObject, Int64, @out Builtin.BridgeObject)
// OPT: } // end sil function '$s22pre_specialize_layouts40usePresepcializedMultipleIndirectResults___2xs2ysy0a20_specialized_module_C09SomeClassC_AA0m5OtherN0Cs5Int64VSaySiGSaySfGtF'
public final class SomeOtherClass {}
public func usePresepcializedMultipleIndirectResults(_ c: SomeClass, _ d: SomeOtherClass, _ x: Int64, xs: [Int], ys: [Float]) {
  consume(publicPresepcializedMultipleIndirectResults(c, d, x))
  consume(publicPresepcializedMultipleIndirectResults(xs, ys, x))
}

// OPT: sil @$s22pre_specialize_layouts58usePresepcializedMultipleIndirectResultsWithMarkerProtocolyy0a20_specialized_module_C09SomeClassC_AA0n5OtherO0Cs5Int64VtF : $@convention(thin) (@guaranteed SomeClass, @guaranteed SomeOtherClass, Int64) -> () {
// OPT: {{bb.*}}([[P1:%.*]] : $SomeClass, [[P2:%.*]] : $SomeOtherClass, [[P3:%.*]] : $Int64):
// OPT:   [[R1:%.*]] = alloc_stack $SomeOtherClass
// OPT:   [[R2:%.*]] = alloc_stack $SomeClass
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts61publicPresepcializedMultipleIndirectResultsWithMarkerProtocolyq__s5Int64Vxtx_q_ADts8SendableRzr0_lFyXl_yXlTs5 : $@convention(thin) (@guaranteed AnyObject, @guaranteed AnyObject, Int64) -> (@out AnyObject, Int64, @out AnyObject)
// OPT:   [[R3:%.*]] = unchecked_addr_cast [[R1]] : $*SomeOtherClass to $*AnyObject
// OPT:   [[R4:%.*]] = unchecked_addr_cast [[R2]] : $*SomeClass to $*AnyObject
// OPT:   [[A1:%.*]] = unchecked_ref_cast [[P1]] : $SomeClass to $AnyObject
// OPT:   [[A2:%.*]] = unchecked_ref_cast [[P2]] : $SomeOtherClass to $AnyObject
// OPT:   apply [[F1]]([[R3]], [[R4]], [[A1]], [[A2]], [[P3]]) : $@convention(thin) (@guaranteed AnyObject, @guaranteed AnyObject, Int64) -> (@out AnyObject, Int64, @out AnyObject)
// OPT: } // end sil function '$s22pre_specialize_layouts58usePresepcializedMultipleIndirectResultsWithMarkerProtocolyy0a20_specialized_module_C09SomeClassC_AA0n5OtherO0Cs5Int64VtF'
public func usePresepcializedMultipleIndirectResultsWithMarkerProtocol(_ c: SomeClass, _ d: SomeOtherClass, _ x: Int64) {
  consume(publicPresepcializedMultipleIndirectResultsWithMarkerProtocol(c, d, x))
}

// OPT: sil [noinline] @$s22pre_specialize_layouts15usePartialApply1y0a20_specialized_module_C09SomeClassCAFcAF_tF : $@convention(thin) (@guaranteed SomeClass) -> @owned @callee_guaranteed (@guaranteed SomeClass) -> @owned SomeClass {
// OPT:   [[F1:%.*]] = function_ref @$s22pre_specialize_layouts15usePartialApply1y0a20_specialized_module_C09SomeClassCAFcAF_tF0deF5InnerL_1xxx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject, @guaranteed SomeClass) -> @owned AnyObject
// OPT:   [[R1:%.*]] = partial_apply [callee_guaranteed] [[F1]]({{%.*}}) : $@convention(thin) (@guaranteed AnyObject, @guaranteed SomeClass) -> @owned AnyObject
// OPT:   [[R2:%.*]] = convert_function [[R1]] : $@callee_guaranteed (@guaranteed AnyObject) -> @owned AnyObject to $@callee_guaranteed (@guaranteed SomeClass) -> @owned SomeClass
// OPT: } // end sil function '$s22pre_specialize_layouts15usePartialApply1y0a20_specialized_module_C09SomeClassCAFcAF_tF'
@inline(never)
public func usePartialApply(y: SomeClass) -> (SomeClass) -> SomeClass {
  @inline(never)
  @_specialize(exported: true, where @_noMetadata T : _Class)
  func usePartialApplyInner<T>(x: T) -> T {
    consume(y)
    return x
  }

  return usePartialApplyInner
}

// NOTE: AnyObject specializations MUST not be applied to existential references.

// OPT: sil @$s22pre_specialize_layouts48useLayoutPrespecializedEntryPointWithExistentialyyAA21SomeReferenceProtocol_pF : $@convention(thin) (@guaranteed any SomeReferenceProtocol) -> () {
// OPT:   {{bb.*}}([[A1:%.*]] : $any SomeReferenceProtocol):
// OPT-NOT: {{%.*}} = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT: } // end sil function '$s22pre_specialize_layouts48useLayoutPrespecializedEntryPointWithExistentialyyAA21SomeReferenceProtocol_pF'
public protocol SomeReferenceProtocol: AnyObject {}
public func useLayoutPrespecializedEntryPointWithExistential(_ p: any SomeReferenceProtocol) {
  publicPrespecialized(p)
}

// OPT-macosx: sil [available 50] @$s22pre_specialize_layouts40usePrespecializedEntryPointsAvailabilityyyF : $@convention(thin) () -> () {
// OPT-macosx:  [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFAA8SomeDataV_Ts5 : $@convention(thin) (SomeData) -> ()
// OPT-macosx:  apply [[F1]](
// OPT-macosx:  [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT-macosx:  [[A1:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT-macosx:  apply [[F2]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT-macosx: } // end sil function '$s22pre_specialize_layouts40usePrespecializedEntryPointsAvailabilityyyF'
@available(macOS 50, *)
public func usePrespecializedEntryPointsAvailability() {
  publicPrespecialized(SomeData())
  publicPrespecialized(SomeClass())
}
// OPT: sil @$s30pre_specialized_module_layouts16publicInlineableyyxlFSd_Ts5 : $@convention(thin) (Double) -> () {
// NONE: sil @$s30pre_specialized_module_layouts16publicInlineableyyxlFSd_Ts5 : $@convention(thin) (Double) -> () {
@_specialize(exported: true, target: publicInlineable(_:), where T == Double)
public func specializeTarget<T>(_ t: T) {}
