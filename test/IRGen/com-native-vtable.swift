// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -module-name COM -emit-module-path %t/COM.swiftmodule %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s | %FileCheck %s

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol IIndependent {
}

@com
public final class COMObject: IDerived, IIndependent {
  public init() {
  }
}

// Emit one vtable per physical projection. Every vtable starts with the shared
// interface map, its adjustment to the native object, and the three common COM
// operations. ISwiftObject then adds its default property witnesses.

// CHECK-DAG: @"$s{{.*}}9COMObjectCMn.com.vtable.$s3COM12ISwiftObjectMp"   = private constant {{.*}} { ptr @"$s{{.*}}9COMObjectCMn.com.interface_map", i64  8, ptr @QueryInterface, ptr @AddRef, ptr @Release, ptr @"$s{{.*}}9COMObjectC{{.*}}ISwiftObject{{.*}}6object{{.*}}TW.com.entry", ptr @"$s{{.*}}9COMObjectC{{.*}}ISwiftObject{{.*}}8metadata{{.*}}TW.com.entry"
// CHECK-DAG: @"$s{{.*}}9COMObjectCMn.com.vtable.$s{{.*}}8IDerivedMp"      = private constant {{.*}} { ptr @"$s{{.*}}9COMObjectCMn.com.interface_map", i64 16, ptr @QueryInterface, ptr @AddRef, ptr @Release
// CHECK-DAG: @"$s{{.*}}9COMObjectCMn.com.vtable.$s{{.*}}12IIndependentMp" = private constant {{.*}} { ptr @"$s{{.*}}9COMObjectCMn.com.interface_map", i64 24, ptr @QueryInterface, ptr @AddRef, ptr @Release

// CHECK-DAG: define internal ptr @"$s{{.*}}9COMObjectC{{.*}}ISwiftObject{{.*}}6object{{.*}}TW.com.entry"(ptr
// CHECK-DAG: define internal ptr @"$s{{.*}}9COMObjectC{{.*}}ISwiftObject{{.*}}8metadata{{.*}}TW.com.entry"(ptr

// The prefix template stores the vtable address points in memory order:
// IIndependent, IDerived (which also represents IBase), then ISwiftObject
// closest to the native object.

// CHECK-DAG: @"$s{{.*}}9COMObjectCMn.com.prefix" = private constant [3 x ptr] [ptr getelementptr inbounds ({{.*}}, ptr @"$s{{.*}}9COMObjectCMn.com.vtable.$s4main12IIndependentMp", i32 0, i32 2), ptr getelementptr inbounds ({{.*}}}, ptr @"$s{{.*}}9COMObjectCMn.com.vtable.$s4main8IDerivedMp", i32 0, i32 2), ptr getelementptr inbounds ({{.*}}, ptr @"$s{{.*}}9COMObjectCMn.com.vtable.$s3COM12ISwiftObjectMp", i32 0, i32 2)]
