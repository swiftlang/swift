// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -emit-module-path %t/pre_specialized_module_layouts.swiftmodule %S/Inputs/pre_specialized_module_layouts.swift
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s --check-prefix=OPT -check-prefix=OPT-%target-os
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -I %t -Onone -emit-sil %s | %FileCheck %s --check-prefix=NONE -check-prefix=NONE-%target-os


// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -O -emit-module-path %t/pre_specialized_module_layouts.swiftmodule %S/Inputs/pre_specialized_module_layouts.swift
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s --check-prefix=OPT

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -O -enable-library-evolution -emit-module-path %t/pre_specialized_module_layouts.swiftmodule %S/Inputs/pre_specialized_module_layouts.swift
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s --check-prefix=OPT

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -O -swift-version 5 -enable-library-evolution -emit-module -o /dev/null -emit-module-interface-path %t/pre_specialized_module_layouts.swiftinterface %S/Inputs/pre_specialized_module_layouts.swift -module-name pre_specialized_module_layouts
// RUN: %target-swift-frontend -enable-experimental-feature LayoutPrespecialization -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s --check-prefix=OPT

// REQUIRES: asserts

import pre_specialized_module_layouts

// Helper to prevent return values from getting optimized away
@inline(never)
public func consume<T>(_ x: T) {}

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
@_specialize(exported: true, availability: macOS 10.5, *; where T == Double)
@_alwaysEmitIntoClient
internal func testEmitIntoClient<T>(t: T) {
  print(t)
}

// OPT: sil @$s22pre_specialize_layouts28usePrespecializedEntryPointsyyF : $@convention(thin) () -> () {
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFSi_Ts5 : $@convention(thin) (Int) -> ()
// OPT:   apply [[F1]]
// OPT:   [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFSd_Ts5 : $@convention(thin) (Double) -> ()
// OPT:   apply [[F2]]
// OPT-macosx:   [[F6:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// OPT-macosx:    apply [[F6]]<SomeData>
// OPT: [[F7:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT: [[A1:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT: apply [[F7]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT:   [[F3:%.*]] = function_ref @$s30pre_specialized_module_layouts36internalEmitIntoClientPrespecializedyyxlFSi_Ts5 : $@convention(thin) (Int) -> ()
// OPT:   apply [[F3]]
// OPT:   [[F4:%.*]] = function_ref @$s30pre_specialized_module_layouts36internalEmitIntoClientPrespecializedyyxlFSd_Ts5 : $@convention(thin) (Double) -> ()
// OPT:   apply [[F4]]
// OPT:   [[F5:%.*]] = function_ref @$s30pre_specialized_module_layouts16useInternalThingyyxlFSi_Tg5
// OPT:   apply [[F5]]({{.*}}) : $@convention(thin) (Int) -> ()
// OPT: } // end sil function '$s22pre_specialize_layouts28usePrespecializedEntryPointsyyF'

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
// OPT:   [[A1:%.*]] = unchecked_bitwise_cast {{%.*}} : $InternalThing2<SomeClass> to $InternalThing2<AnyObject>
// OPT:   [[R3:%.*]] = apply [[F1]]([[A1]]) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   store [[R3]] to [[R2]] : $*AnyObject
// OPT:   [[R4:%.*]] = init_existential_addr {{%.*}} : $*Any, $SomeClass
// OPT:   [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedXxvgyXl_Ts5 : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[R5:%.*]] = unchecked_addr_cast [[R4]] : $*SomeClass to $*AnyObject
// OPT:   [[A2:%.*]] = unchecked_bitwise_cast {{%.*}} : $InternalThing2<SomeClass> to $InternalThing2<AnyObject>
// OPT:   [[R6:%.*]] = apply [[F2]]([[A2]]) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   store [[R6]] to [[R5]] : $*AnyObject
// OPT:   [[F3:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedYxvsyXl_Ts5 : $@convention(method) (@owned AnyObject, @inout InternalThing2<AnyObject>) -> ()
// OPT:   [[A3:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT:   [[A4:%.*]] = unchecked_addr_cast {{%.*}} : $*InternalThing2<SomeClass> to $*InternalThing2<AnyObject>
// OPT:   apply [[F3]]([[A3]], [[A4]]) : $@convention(method) (@owned AnyObject, @inout InternalThing2<AnyObject>) -> ()
// OPT:   [[F4:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedYxvgyXl_Ts5 : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[A5:%.*]] = unchecked_bitwise_cast {{%.*}} : $InternalThing2<SomeClass> to $InternalThing2<AnyObject>
// OPT:   [[R5:%.*]] = apply [[F4]]([[A5]]) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[F5:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedZxvMyXl_Ts5 : $@yield_once @convention(method) (@inout InternalThing2<AnyObject>) -> @yields @inout AnyObject
// OPT:   ([[R7:%.*]], {{%.*}}) = begin_apply [[F5]]([[A4]]) : $@yield_once @convention(method) (@inout InternalThing2<AnyObject>) -> @yields @inout AnyObject
// OPT:   [[R8:%.*]] = unchecked_addr_cast [[R7]] : $*AnyObject to $*SomeClass
// OPT:   [[F6:%.*]] = function_ref @$s30pre_specialized_module_layouts14InternalThing2V9computedZxvryXl_Ts5 : $@yield_once @convention(method) (@guaranteed InternalThing2<AnyObject>) -> @yields @in_guaranteed AnyObject
// OPT:   [[A6:%.*]] = unchecked_bitwise_cast {{%.*}} : $InternalThing2<SomeClass> to $InternalThing2<AnyObject>
// OPT:   ([[R9:%.*]], {{%.*}}) = begin_apply [[F6]]([[A6]]) : $@yield_once @convention(method) (@guaranteed InternalThing2<AnyObject>) -> @yields @in_guaranteed AnyObject
// OPT:   [[R10:%.*]] = unchecked_addr_cast [[R9]] : $*AnyObject to $*SomeClass
// OPT: } // end sil function '$s30pre_specialized_module_layouts16useInternalThingyyxlFAA9SomeClassC_Tg5'

public func usePrespecializedEntryPoints() {
  publicPrespecialized(1)
  publicPrespecialized(1.0)
  publicPrespecialized(SomeData())
  publicPrespecialized(SomeClass())
  useInternalEmitIntoClientPrespecialized(2)
  useInternalEmitIntoClientPrespecialized(2.0)
  useInternalThing(2)
  useInternalThing(SomeClass())
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

// OPT: {{bb.*}}([[A1:%.*]] : $SomeClass, [[A2:%.*]] : $SomeOtherClass, [[A3:%.*]] : $Int64):
// OPT:   [[R1:%.*]] = alloc_stack $SomeOtherClass
// OPT:   [[R2:%.*]] = alloc_stack $SomeClass
// OPT:   [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts43publicPresepcializedMultipleIndirectResultsyq__s5Int64Vxtx_q_ADtr0_lFyXl_yXlTs5 : $@convention(thin) (@guaranteed AnyObject, @guaranteed AnyObject, Int64) -> (@out AnyObject, Int64, @out AnyObject)
// OPT:   [[R3:%.*]] = unchecked_addr_cast [[R1]] : $*SomeOtherClass to $*AnyObject
// OPT:   [[R4:%.*]] = unchecked_addr_cast [[R2]] : $*SomeClass to $*AnyObject
// OPT:   [[A4:%.*]] = unchecked_ref_cast [[A1]] : $SomeClass to $AnyObject
// OPT:   [[A5:%.*]] = unchecked_ref_cast [[A2]] : $SomeOtherClass to $AnyObject
// OPT:   [[R5:%.*]] = apply [[F1]]([[R3]], [[R4]], [[A4]], [[A5]], [[A3]]) : $@convention(thin) (@guaranteed AnyObject, @guaranteed AnyObject, Int64) -> (@out AnyObject, Int64, @out AnyObject)
// OPT: } // end sil function '$s22pre_specialize_layouts40usePresepcializedMultipleIndirectResultsyy0a20_specialized_module_C09SomeClassC_AA0k5OtherL0Cs5Int64VtF'
public final class SomeOtherClass {}
public func usePresepcializedMultipleIndirectResults(_ c: SomeClass, _ d: SomeOtherClass, _ x: Int64) {
  consume(publicPresepcializedMultipleIndirectResults(c, d, x))
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

// OPT-macosx: sil [available 10.50] @$s22pre_specialize_layouts40usePrespecializedEntryPointsAvailabilityyyF : $@convention(thin) () -> () {
// OPT-macosx:  [[F1:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFAA8SomeDataV_Ts5 : $@convention(thin) (SomeData) -> ()
// OPT-macosx:  apply [[F1]](
// OPT-macosx:  [[F2:%.*]] = function_ref @$s30pre_specialized_module_layouts20publicPrespecializedyyxlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT-macosx:  [[A1:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT-macosx:  apply [[F2]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT-macosx: } // end sil function '$s22pre_specialize_layouts40usePrespecializedEntryPointsAvailabilityyyF'
@available(macOS 10.50, *)
public func usePrespecializedEntryPointsAvailability() {
  publicPrespecialized(SomeData())
  publicPrespecialized(SomeClass())
}
// OPT: sil @$s30pre_specialized_module_layouts16publicInlineableyyxlFSd_Ts5 : $@convention(thin) (Double) -> () {
// NONE: sil @$s30pre_specialized_module_layouts16publicInlineableyyxlFSd_Ts5 : $@convention(thin) (Double) -> () {
@_specialize(exported: true, target: publicInlineable(_:), where T == Double)
public func specializeTarget<T>(_ t: T) {}
