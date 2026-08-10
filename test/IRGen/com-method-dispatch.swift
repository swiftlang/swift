// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-experimental-com-interop -emit-module-path %t/COM.swiftmodule -module-name COM %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -enable-experimental-com-interop -I %t -emit-ir %s | %FileCheck %s -implicit-check-not swift_getExistentialTypeMetadata

@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IBase {
  func method(_ value: CInt) -> CInt
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IDerived: IBase {
  func derived(_ value: CInt) -> CInt
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}4base
// CHECK-SAME:  (ptr [[ARG:%.*]], i32 [[VALUE:%.*]])
// CHECK:         call void @llvm.lifetime.start{{.*}}(i64 8, ptr [[STORAGE:%.*]])
// CHECK-NEXT:    store ptr [[ARG]], ptr [[STORAGE]]
// CHECK:         [[INTERFACE:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK:         [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
// CHECK:         [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 3
// CHECK:         [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK:         [[SELF:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK-NOT:     call swiftcc
// CHECK:         call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]], i32 [[VALUE]])
// CHECK-NOT:     swift_getWitnessTable
public func base(_ interface: borrowing any IBase, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}7refined
// CHECK-SAME:  (ptr [[ARG:%.*]], i32 [[VALUE:%.*]])
// CHECK:         call void @llvm.lifetime.start{{.*}}(i64 8, ptr [[STORAGE:%.*]])
// CHECK-NEXT:    store ptr [[ARG]], ptr [[STORAGE]]
// CHECK:         [[INTERFACE:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK:         [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
// CHECK:         [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 3
// CHECK:         [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK:         [[SELF:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK-NOT:     call swiftcc
// CHECK:         call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]], i32 [[VALUE]])
// CHECK-NOT:     swift_getWitnessTable
public func refined(_ interface: borrowing any IDerived, _ value: CInt) -> CInt {
  interface.method(value)
}

// CHECK-LABEL: define{{.*}} swiftcc i32 @"$s{{.*}}7derived
// CHECK-SAME:  (ptr [[ARG:%.*]], i32 [[VALUE:%.*]])
// CHECK:         call void @llvm.lifetime.start{{.*}}(i64 8, ptr [[STORAGE:%.*]])
// CHECK-NEXT:    store ptr [[ARG]], ptr [[STORAGE]]
// CHECK:         [[INTERFACE:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK:         [[VTABLE:%.*]] = load ptr, ptr [[INTERFACE]]
// CHECK:         [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[VTABLE]], i{{32|64}} 4
// CHECK:         [[METHOD:%.*]] = load ptr, ptr [[SLOT]]
// CHECK:         [[SELF:%.*]] = load ptr, ptr [[STORAGE]]
// CHECK-NOT:     call swiftcc
// CHECK:         call {{(x86_stdcallcc )?}}i32 [[METHOD]](ptr [[SELF]], i32 [[VALUE]])
// CHECK-NOT:     swift_getWitnessTable
public func derived(_ interface: borrowing any IDerived, _ value: CInt) -> CInt {
  interface.derived(value)
}

// CHECK-LABEL: define{{.*}} swiftcc ptr @"$s{{.*}}6opaque
// CHECK:         call ptr {{%.*}}(ptr {{%.*}}
public func opaque(_ interface: borrowing any ISwiftObject) -> UnsafeMutableRawPointer {
  interface.object
}
