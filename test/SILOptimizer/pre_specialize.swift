// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/pre_specialized_module.swiftmodule %S/Inputs/pre_specialized_module.swift
// RUN: %target-swift-frontend -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s --check-prefix=OPT -check-prefix=OPT-%target-os
// RUN: %target-swift-frontend -I %t -Onone -emit-sil %s | %FileCheck %s --check-prefix=NONE -check-prefix=NONE-%target-os


// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -emit-module-path %t/pre_specialized_module.swiftmodule %S/Inputs/pre_specialized_module.swift
// RUN: %target-swift-frontend -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s --check-prefix=OPT

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -enable-library-evolution -emit-module-path %t/pre_specialized_module.swiftmodule %S/Inputs/pre_specialized_module.swift
// RUN: %target-swift-frontend -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s --check-prefix=OPT

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -O -swift-version 5 -enable-library-evolution -emit-module -o /dev/null -emit-module-interface-path %t/pre_specialized_module.swiftinterface %S/Inputs/pre_specialized_module.swift -module-name pre_specialized_module
// RUN: %target-swift-frontend -I %t -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil %s | %FileCheck %s --check-prefix=OPT

import pre_specialized_module

// Helper to prevent return values from getting optimized away
@inline(never)
public func consume<T>(_ x: T) {}

// Make sure we generate the public pre-specialized entry points.

// OPT-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// OPT-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// OPT-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> () {
// OPT-macosx-DAG: sil [available 10.5] @$s14pre_specialize10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-linux-gnu-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {

// NONE-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// NONE-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// NONE-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> () {
// NONE-macosx-DAG: sil [available 10.5] @$s14pre_specialize10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// NONE-linux-gnu-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {

@_specialize(exported: true, where T == Int)
@_specialize(exported: true, where T == Float)
@_specialize(exported: true, where @_noMetadata T : _Class)
@_specialize(exported: true, availability: macOS 10.5, *; where T == Double)
public func testPublic<T>(t: T) {
  print(t)
}

// OPT-macosx-DAG: sil [available 10.5] @$s14pre_specialize18testEmitIntoClient1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-linux-gnu-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// OPT-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// OPT-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> () {

// NONE-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// NONE-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// NONE-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> () {

@_specialize(exported: true, where T == Int)
@_specialize(exported: true, where T == Float)
@_specialize(exported: true, where @_noMetadata T : _Class)
@_specialize(exported: true, availability: macOS 10.5, *; where T == Double)
@_alwaysEmitIntoClient
internal func testEmitIntoClient<T>(t: T) {
  print(t)
}

// OPT: sil @$s14pre_specialize28usePrespecializedEntryPointsyyF : $@convention(thin) () -> () {
// OPT:   [[F1:%.*]] = function_ref @$s22pre_specialized_module20publicPrespecializedyyxlFSi_Ts5 : $@convention(thin) (Int) -> ()
// OPT:   apply [[F1]]
// OPT:   [[F2:%.*]] = function_ref @$s22pre_specialized_module20publicPrespecializedyyxlFSd_Ts5 : $@convention(thin) (Double) -> ()
// OPT:   apply [[F2]]
// OPT-macosx:   [[F6:%.*]] = function_ref @$s22pre_specialized_module20publicPrespecializedyyxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> ()
// OPT-macosx:    apply [[F6]]<SomeData>
// OPT: [[F7:%.*]] = function_ref @$s22pre_specialized_module20publicPrespecializedyyxlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT: [[A1:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT: apply [[F7]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT:   [[F3:%.*]] = function_ref @$s22pre_specialized_module36internalEmitIntoClientPrespecializedyyxlFSi_Ts5 : $@convention(thin) (Int) -> ()
// OPT:   apply [[F3]]
// OPT:   [[F4:%.*]] = function_ref @$s22pre_specialized_module36internalEmitIntoClientPrespecializedyyxlFSd_Ts5 : $@convention(thin) (Double) -> ()
// OPT:   apply [[F4]]
// OPT:   [[F5:%.*]] = function_ref @$s22pre_specialized_module16useInternalThingyyxlFSi_Tg5
// OPT:   apply [[F5]]({{.*}}) : $@convention(thin) (Int) -> ()
// OPT: } // end sil function '$s14pre_specialize28usePrespecializedEntryPointsyyF'

// OPT: sil {{.*}} @$s22pre_specialized_module16useInternalThingyyxlFSi_Tg5 : $@convention(thin) (Int) -> () {
// OPT:   [[F1:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V7computexyFSi_Ts5 : $@convention(method) (InternalThing2<Int>) -> Int
// OPT:   apply [[F1]](
// OPT:   [[F2:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedXxvgSi_Ts5 : $@convention(method) (InternalThing2<Int>) -> Int
// OPT:   apply [[F2]](
// OPT:   [[F3:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedYxvsSi_Ts5 : $@convention(method) (Int, @inout InternalThing2<Int>) -> ()
// OPT:   apply [[F3]](
// OPT:   [[F4:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedYxvgSi_Ts5 : $@convention(method) (InternalThing2<Int>) -> Int
// OPT:   apply [[F4]](
// OPT:   [[F5:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedZxvMSi_Ts5 : $@yield_once @convention(method) (@inout InternalThing2<Int>) -> @yields @inout Int
// OPT:   begin_apply [[F5]](
// OPT:   [[F6:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedZxvrSi_Ts5 : $@yield_once @convention(method) (InternalThing2<Int>) -> @yields @in_guaranteed Int
// OPT:   begin_apply [[F6]](
// OPT:   [[F7:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2VyxSicisSi_Ts5 : $@convention(method) (Int, Int, @inout InternalThing2<Int>) -> ()
// OPT:   apply [[F7]](
// OPT:   [[F8:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2VyxSicigSi_Ts5 : $@convention(method) (Int, InternalThing2<Int>) -> Int
// OPT:   apply [[F8]](
// OPT: } // end sil function '$s22pre_specialized_module16useInternalThingyyxlFSi_Tg5'

// OPT: sil shared @$s22pre_specialized_module16useInternalThingyyxlFAA9SomeClassC_Tg5 : $@convention(thin) (@guaranteed SomeClass) -> () {
// OPT:   [[F1:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V7computexyFyXl_Ts5 : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[A1:%.*]] = unchecked_bitwise_cast {{%.*}} : $InternalThing2<SomeClass> to $InternalThing2<AnyObject>
// OPT:   [[R1:%.*]] = apply [[F1]]([[A1]]) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[R2:%.*]] = unchecked_ref_cast [[R1]] : $AnyObject to $SomeClass
// OPT:   [[F2:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedXxvgyXl_Ts5 : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[A2:%.*]] = unchecked_bitwise_cast {{%.*}} : $InternalThing2<SomeClass> to $InternalThing2<AnyObject>
// OPT:   [[R3:%.*]] = apply [[F2]]([[A2]]) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[R4:%.*]] = unchecked_ref_cast [[R3]] : $AnyObject to $SomeClass
// OPT:   [[F3:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedYxvsyXl_Ts5 : $@convention(method) (@owned AnyObject, @inout InternalThing2<AnyObject>) -> ()
// OPT:   [[A3:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT:   [[A4:%.*]] = unchecked_addr_cast {{%.*}} : $*InternalThing2<SomeClass> to $*InternalThing2<AnyObject>
// OPT:   apply [[F3]]([[A3]], [[A4]]) : $@convention(method) (@owned AnyObject, @inout InternalThing2<AnyObject>) -> ()
// OPT:   [[F4:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedYxvgyXl_Ts5 : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[A5:%.*]] = unchecked_bitwise_cast {{%.*}} : $InternalThing2<SomeClass> to $InternalThing2<AnyObject>
// OPT:   [[R5:%.*]] = apply [[F4]]([[A5]]) : $@convention(method) (@guaranteed InternalThing2<AnyObject>) -> @owned AnyObject
// OPT:   [[R6:%.*]] = unchecked_ref_cast [[R5]] : $AnyObject to $SomeClass
// OPT:   [[F5:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedZxvMyXl_Ts5 : $@yield_once @convention(method) (@inout InternalThing2<AnyObject>) -> @yields @inout AnyObject
// OPT:   ([[R7:%.*]], {{%.*}}) = begin_apply [[F5]]([[A4]]) : $@yield_once @convention(method) (@inout InternalThing2<AnyObject>) -> @yields @inout AnyObject
// OPT:   [[R8:%.*]] = unchecked_addr_cast [[R7]] : $*AnyObject to $*SomeClass
// OPT:   [[F6:%.*]] = function_ref @$s22pre_specialized_module14InternalThing2V9computedZxvryXl_Ts5 : $@yield_once @convention(method) (@guaranteed InternalThing2<AnyObject>) -> @yields @in_guaranteed AnyObject
// OPT:   [[A6:%.*]] = unchecked_bitwise_cast {{%.*}} : $InternalThing2<SomeClass> to $InternalThing2<AnyObject>
// OPT:   ([[R9:%.*]], {{%.*}}) = begin_apply [[F6]]([[A6]]) : $@yield_once @convention(method) (@guaranteed InternalThing2<AnyObject>) -> @yields @in_guaranteed AnyObject
// OPT:   [[R10:%.*]] = unchecked_addr_cast [[R9]] : $*AnyObject to $*SomeClass
// OPT: } // end sil function '$s22pre_specialized_module16useInternalThingyyxlFAA9SomeClassC_Tg5'

public func usePrespecializedEntryPoints() {
  publicPrespecialized(1)
  publicPrespecialized(1.0)
  publicPrespecialized(SomeData())
  publicPrespecialized(SomeClass())
  useInternalEmitIntoClientPrespecialized(2)
  useInternalEmitIntoClientPrespecialized(2.0)
  useInternalThing(2)
  // FIXME: begin_apply is broken with AnyObject specialization
  useInternalThing(SomeClass())
}

// OPT: sil @$s14pre_specialize34usePrespecializedThrowsEntryPointsyyKF : $@convention(thin) () -> @error Error {
// OPT:   [[F1:%.*]] = function_ref @$s22pre_specialized_module26publicPrespecializedThrowsyxxKlFSi_Ts5 : $@convention(thin) (Int) -> (Int, @error Error)
// OPT:   try_apply [[F1]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error Error)
// OPT-macosx:   [[F2:%.*]] = function_ref @$s22pre_specialized_module26publicPrespecializedThrowsyxxKlFAA8SomeDataV_Tg5 : $@convention(thin) (SomeData) -> (SomeData, @error Error)
// OPT-macosx:   try_apply [[F2]]({{%.*}}) : $@convention(thin) (SomeData) -> (SomeData, @error Error)
// OPT:   [[F3:%.*]] = function_ref @$s22pre_specialized_module26publicPrespecializedThrowsyxxKlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> (@owned AnyObject, @error Error)
// OPT:   [[A1:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT:   try_apply [[F3]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> (@owned AnyObject, @error Error), normal [[BB1:bb.*]], error
// OPT: [[BB1]]([[A2:%.*]] : $AnyObject):
// OPT:   unchecked_ref_cast [[A2]] : $AnyObject to $SomeClass
// OPT: } // end sil function '$s14pre_specialize34usePrespecializedThrowsEntryPointsyyKF'
public func usePrespecializedThrowsEntryPoints() throws {
  consume(try publicPrespecializedThrows(1))
  consume(try publicPrespecializedThrows(SomeData()))
  consume(try publicPrespecializedThrows(SomeClass()))
}

// FIXME: Currently this causes a compiler crash
//
// public func useT(_ c: SomeClass, _ d: SomeOtherClass, _ x: Int64) {
//   consume(publicPresepcializedMultipleIndirectResults(c, d, x))
// }

// OPT: sil [noinline] @$s14pre_specialize15usePartialApply1y0A19_specialized_module9SomeClassCAFcAF_tF : $@convention(thin) (@guaranteed SomeClass) -> @owned @callee_guaranteed (@guaranteed SomeClass) -> @owned SomeClass {
// OPT:   [[F1:%.*]] = function_ref @$s14pre_specialize15usePartialApply1y0A19_specialized_module9SomeClassCAFcAF_tF0cdE5InnerL_1xxx_tlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject, @guaranteed SomeClass) -> @owned AnyObject
// OPT:   [[R1:%.*]] = partial_apply [callee_guaranteed] [[F1]]({{%.*}}) : $@convention(thin) (@guaranteed AnyObject, @guaranteed SomeClass) -> @owned AnyObject
// OPT:   [[R2:%.*]] = convert_function [[R1]] : $@callee_guaranteed (@guaranteed AnyObject) -> @owned AnyObject to $@callee_guaranteed (@guaranteed SomeClass) -> @owned SomeClass
// OPT: } // end sil function '$s14pre_specialize15usePartialApply1y0A19_specialized_module9SomeClassCAFcAF_tF'
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

// OPT-macosx: sil [available 10.50] @$s14pre_specialize40usePrespecializedEntryPointsAvailabilityyyF : $@convention(thin) () -> () {
// OPT-macosx:  [[F1:%.*]] = function_ref @$s22pre_specialized_module20publicPrespecializedyyxlFAA8SomeDataV_Ts5 : $@convention(thin) (SomeData) -> ()
// OPT-macosx:  apply [[F1]](
// OPT-macosx:  [[F2:%.*]] = function_ref @$s22pre_specialized_module20publicPrespecializedyyxlFyXl_Ts5 : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT-macosx:  [[A1:%.*]] = unchecked_ref_cast {{%.*}} : $SomeClass to $AnyObject
// OPT-macosx:  apply [[F2]]([[A1]]) : $@convention(thin) (@guaranteed AnyObject) -> ()
// OPT-macosx: } // end sil function '$s14pre_specialize40usePrespecializedEntryPointsAvailabilityyyF'
@available(macOS 10.50, *)
public func usePrespecializedEntryPointsAvailability() {
  publicPrespecialized(SomeData())
  publicPrespecialized(SomeClass())
}
// OPT: sil @$s22pre_specialized_module16publicInlineableyyxlFSd_Ts5 : $@convention(thin) (Double) -> () {
// NONE: sil @$s22pre_specialized_module16publicInlineableyyxlFSd_Ts5 : $@convention(thin) (Double) -> () {
@_specialize(exported: true, target: publicInlineable(_:), where T == Double)
public func specializeTarget<T>(_ t: T) {}
