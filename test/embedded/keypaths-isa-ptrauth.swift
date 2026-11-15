// Check that the isa of a statically-emitted embedded key path object is signed
// the way the runtime authenticates it: DA key, address-discriminated on the
// object itself, with the isa discriminator (0x6ae1). An unsigned isa traps the
// first time the key path is used.

// RUN: %target-swift-emit-ir %s -module-name main -enable-experimental-feature Embedded -wmo -parse-as-library | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: swift_feature_Embedded

public struct S { public var a: Int32 = 0; public var b: Int32 = 0 }
public final class C { public var v: Int32 = 0 }

// CHECK: @"$es15WritableKeyPathCy4main1SVs5Int32VGMf.ptrauth" = private constant { ptr, i32, i64, i64 } { ptr getelementptr inbounds ({{.*}}, ptr @"$es15WritableKeyPathCy4main1SVs5Int32VGMf", i32 0, i32 1), i32 2, i64 ptrtoint (ptr [[STRUCT_KP:@keypath[.0-9]*]] to i64), i64 27361 }, section "llvm.ptrauth"
// CHECK: [[STRUCT_KP]] = private constant { %swift.refcounted, {{.*}} } { %swift.refcounted { ptr @"$es15WritableKeyPathCy4main1SVs5Int32VGMf.ptrauth", i64 -1 },
public func kpStruct() -> WritableKeyPath<S, Int32> { \S.b }

// CHECK: @"$es24ReferenceWritableKeyPathCy4main1CCs5Int32VGMf.ptrauth" = private constant { ptr, i32, i64, i64 } { ptr getelementptr inbounds ({{.*}}, ptr @"$es24ReferenceWritableKeyPathCy4main1CCs5Int32VGMf", i32 0, i32 1), i32 2, i64 ptrtoint (ptr [[CLASS_KP:@keypath[.0-9]*]] to i64), i64 27361 }, section "llvm.ptrauth"
// CHECK: [[CLASS_KP]] = private constant { %swift.refcounted, {{.*}} } { %swift.refcounted { ptr @"$es24ReferenceWritableKeyPathCy4main1CCs5Int32VGMf.ptrauth", i64 -1 },
public func kpClass() -> ReferenceWritableKeyPath<C, Int32> { \C.v }
