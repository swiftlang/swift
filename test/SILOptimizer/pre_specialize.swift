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
// OPT-macosx-DAG: sil [available 10.5] @$s14pre_specialize10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-linux-gnu-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {

// NONE-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// NONE-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {
// NONE-macosx-DAG: sil [available 10.5] @$s14pre_specialize10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// NONE-linux-gnu-DAG: sil @$s14pre_specialize10testPublic1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {

@_specialize(exported: true, where T == Int)
@_specialize(exported: true, where T == Float)
@_specialize(exported: true, availability: macOS 10.5, *; where T == Double)
public func testPublic<T>(t: T) {
  print(t)
}

// OPT-macosx-DAG: sil [available 10.5] @$s14pre_specialize18testEmitIntoClient1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-linux-gnu-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSd_Ts5 : $@convention(thin) (Double) -> () {
// OPT-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// OPT-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {

// NONE-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSf_Ts5 : $@convention(thin) (Float) -> () {
// NONE-DAG: sil @$s14pre_specialize18testEmitIntoClient1tyx_tlFSi_Ts5 : $@convention(thin) (Int) -> () {

@_specialize(exported: true, where T == Int)
@_specialize(exported: true, where T == Float)
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

public func usePrespecializedEntryPoints() {
  publicPrespecialized(1)
  publicPrespecialized(1.0)
  publicPrespecialized(SomeData())
  useInternalEmitIntoClientPrespecialized(2)
  useInternalEmitIntoClientPrespecialized(2.0)
  useInternalThing(2)
}

// OPT: sil @$s14pre_specialize34usePrespecializedThrowsEntryPointsyyKF : $@convention(thin) () -> @error any Error {
// OPT:   [[F1:%.*]] = function_ref @$s22pre_specialized_module26publicPrespecializedThrowsyxxKlFSi_Ts5 : $@convention(thin) (Int) -> (Int, @error any Error)
// OPT:   try_apply [[F1]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error any Error)
// OPT-macosx:   [[F2:%.*]] = function_ref @$s22pre_specialized_module26publicPrespecializedThrowsyxxKlFAA8SomeDataV_Tg5 : $@convention(thin) (SomeData) -> (SomeData, @error any Error)
// OPT-macosx:   try_apply [[F2]]({{%.*}}) : $@convention(thin) (SomeData) -> (SomeData, @error any Error)
// OPT: } // end sil function '$s14pre_specialize34usePrespecializedThrowsEntryPointsyyKF'
public func usePrespecializedThrowsEntryPoints() throws {
  consume(try publicPrespecializedThrows(1))
  consume(try publicPrespecializedThrows(SomeData()))
}

// OPT-macosx: sil [available 50] @$s14pre_specialize40usePrespecializedEntryPointsAvailabilityyyF : $@convention(thin) () -> () {
// OPT-macosx:  [[F1:%.*]] = function_ref @$s22pre_specialized_module20publicPrespecializedyyxlFAA8SomeDataV_Ts5 : $@convention(thin) (SomeData) -> ()
// OPT-macosx:  apply [[F1]](
// OPT-macosx: } // end sil function '$s14pre_specialize40usePrespecializedEntryPointsAvailabilityyyF'
@available(macOS 50, *)
public func usePrespecializedEntryPointsAvailability() {
  publicPrespecialized(SomeData())
}
// OPT: sil @$s22pre_specialized_module16publicInlineableyyxlFSd_Ts5 : $@convention(thin) (Double) -> () {
// NONE: sil @$s22pre_specialized_module16publicInlineableyyxlFSd_Ts5 : $@convention(thin) (Double) -> () {
@_specialize(exported: true, target: publicInlineable(_:), where T == Double)
public func specializeTarget<T>(_ t: T) {}
