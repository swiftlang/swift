// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/P.swiftmodule -module-name=P %S/../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -module-name A -I %t -enable-profiling-marker-thunks -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name A -I %t -enable-library-evolution -enable-profiling-marker-thunks -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name A -I %t -enable-profiling-marker-thunks -emit-ir %s | %FileCheck %s --check-prefix=ATTRIBUTE -DINT=i%target-ptrsize
// RUN: %target-swift-frontend -module-name A -I %t -disable-profiling-marker-thunks -emit-ir %s | %FileCheck %s --check-prefix=NOTHUNK
// RUN: %target-swift-frontend -module-name A -I %t -emit-ir %s | %FileCheck %s --check-prefix=NOTHUNK

// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: OS=linux-gnu, OS=linux-android, OS=linux-androideabi

// UNSUPPORTED: CPU=arm64e

// NOTHUNK-NOT: __swift_prof_thunk

import P

open class GenericClass<T> {
  var x: T
  public init(_ t: T) {
    x = t
  }

  public func someMethod() -> Int {
    return 20
  }
}

open class PlainClass {
  var x: Int

  public init() {
    x = 10
  }

  public func someMethod() -> Int {
    return 20
  }
}

public protocol SomeProto {
  associatedtype T

  func retAssoc() -> T
  func plain() throws
}

public struct Conformer<T> : SomeProto {
  var x : T

  public init( _ t: T) {
    self.x = t
  }

  public func retAssoc() -> T {
    return x
  }

  public func plain() throws { print(x) }
}

struct SomeGenericThing<T, V> {
  init(_ t: T, _ v: V) {}
}

public struct ResilientConformer<T> : ResilientBaseProtocol {
  var t : T

  public func requirement() -> Int {
    return 5
  }
}

public func generic<T>(_ t: T) {
  print(t)
}
public func test() {
  generic(SomeGenericThing(1, 3.0))
}

// CHECK: @"$s1A9ConformerVyxGAA9SomeProtoAAWp" = {{.*}} @"__swift_prof_thunk__generic_witness__$s1A9ConformerVyxGAA9SomeProtoA2aEP8retAssoc1TQzyFTW"
// CHECK-SAME: @"__swift_prof_thunk__generic_witness__$s1A9ConformerVyxGAA9SomeProtoA2aEP5plainyyKFTW"

// CHECK: @"$s1A18ResilientConformerVyxG1P0A12BaseProtocolAAMc" = constant {{.*}} @"__swift_prof_thunk__generic_witness__$s1A18ResilientConformerVyxG1P0A12BaseProtocolAaeFP11requirementSiyFTW"

// CHECK: @"$s1A12GenericClassCMn" = constant {{.*}} @"__swift_prof_thunk__generic_vtable__$s1A12GenericClassC1xxvg"
// CHECK-SAME: @"__swift_prof_thunk__generic_vtable__$s1A12GenericClassC10someMethodSiyF"

// CHECK: @"$s1A10PlainClassCMn" = {{.*}}@"$s1A10PlainClassC10someMethodSiyF"

// CHECK: define swiftcc void @"$s1A4testyyF"()
// CHECK:  call swiftcc void @"__swift_prof_thunk__generic_func__2__$sSiN___$sSdN___fun__$s1A16SomeGenericThingVyACyxq_Gx_q_tcfC"
// CHECK:  call swiftcc void @"__swift_prof_thunk__generic_func__1__$s1A16SomeGenericThingVySiSdGN___fun__$s1A7genericyyxlF"
// CHECK:  ret void

// CHECK: define {{.*}} swiftcc void @"__swift_prof_thunk__generic_func__1__$s1A16SomeGenericThingVySiSdGN___fun__$s1A7genericyyxlF"(ptr noalias %0, ptr %1) #[[ATTR:[0-9]+]]
// CHECK: call swiftcc void @"$s1A7genericyyxlF"(ptr noalias %0, ptr %1)
// CHECK: ret

// CHECK: define {{.*}} void @"__swift_prof_thunk__generic_witness__$s1A9ConformerVyxGAA9SomeProtoA2aEP5plainyyKFTW"({{.*}}) #[[ATTR]]{{( comdat)?}} {
// CHECK: define {{.*}} void @"__swift_prof_thunk__generic_vtable__$s1A12GenericClassC1xxvg"({{.*}}) #[[ATTR]]{{( comdat)?}} {

// CHECK: attributes #[[ATTR]] = { noinline "frame-pointer"="all"


// ATTRIBUTE: define {{.*}} ptr @"__swift_prof_thunk__generic_func__1__$sypN___fun__$ss27_finalizeUninitializedArrayySayxGABnlF"({{.*}}) #[[ATTR:[0-9]+]]
// ATTRIBUTE: define {{.*}} void @"__swift_prof_thunk__generic_func__2__$sSiN___$sSdN___fun__$s1A16SomeGenericThingVyACyxq_Gx_q_tcfC"({{.*}}) #[[ATTR]]
// ATTRIBUTE: define {{.*}} void @"__swift_prof_thunk__generic_func__1__$s1A16SomeGenericThingVySiSdGN___fun__$s1A7genericyyxlF"({{.*}}) #[[ATTR]]
// ATTRIBUTE: define {{.*}} void @"__swift_prof_thunk__generic_witness__$s1A9ConformerVyxGAA9SomeProtoA2aEP8retAssoc1TQzyFTW"({{.*}}) #[[ATTR]]
// ATTRIBUTE: define {{.*}} void @"__swift_prof_thunk__generic_witness__$s1A9ConformerVyxGAA9SomeProtoA2aEP5plainyyKFTW"({{.*}}) #[[ATTR]]
// ATTRIBUTE: define {{.*}} [[INT]] @"__swift_prof_thunk__generic_witness__$s1A18ResilientConformerVyxG1P0A12BaseProtocolAaeFP11requirementSiyFTW"({{.*}}) #[[ATTR]]
// ATTRIBUTE: define {{.*}} void @"__swift_prof_thunk__generic_vtable__$s1A12GenericClassC1xxvg"({{.*}}) #[[ATTR]]
// ATTRIBUTE: define {{.*}} void @"__swift_prof_thunk__generic_vtable__$s1A12GenericClassC1xxvs"({{.*}}) #[[ATTR]]
// ATTRIBUTE: define {{.*}} ptr @"__swift_prof_thunk__generic_vtable__$s1A12GenericClassCyACyxGxcfC"({{.*}}) #[[ATTR]]
// ATTRIBUTE: define {{.*}} [[INT]] @"__swift_prof_thunk__generic_vtable__$s1A12GenericClassC10someMethodSiyF"({{.*}}) #[[ATTR]]
// ATTRIBUTE: #[[ATTR]] = { noinline "frame-pointer"="all"
