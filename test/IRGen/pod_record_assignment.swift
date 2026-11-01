// RUN: %target-swift-frontend %s -O -emit-ir -module-name test | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-ptrsize

public final class Reference {}

public struct POD {
  public var a: Int
  public var b: Int
  public var c: Int
  public var d: Int
  public var e: Int
  public var f: Int
}

public struct Mixed {
  public var pod: POD
  public var reference: Reference
}

public func assignWithCopy(_ destination: inout Mixed, _ source: Mixed) {
  destination = source
}

public func assignWithTake(_ destination: inout Mixed,
                           _ source: consuming Mixed) {
  destination = source
}

// The assignWithCopy witness should memcpy the POD field instead of assigning
// each of its fields, retain the source reference, and release the reference
// previously held by the destination. The six Int fields occupy 24 bytes on a
// 32-bit target and 48 bytes on a 64-bit target.

// CHECK-LABEL: define internal {{.*}}ptr @"$s4test5MixedVwca"
// CHECK-32: call void @llvm.memcpy.p0.p0.i32({{.*}}i32 24, i1 false)
// CHECK-64: call void @llvm.memcpy.p0.p0.i64({{.*}}i64 48, i1 false)
// CHECK: call ptr @swift_retain
// CHECK: call void @swift_release
// CHECK: ret ptr

// The assignWithTake should also memcopy,  but the witness transfers
//the source reference rather than retaining it, and only releases the reference displaced at the destination.

// CHECK-LABEL: define internal {{.*}}ptr @"$s4test5MixedVwta"
// CHECK-32: call void @llvm.memcpy.p0.p0.i32({{.*}}i32 24, i1 false)
// CHECK-64: call void @llvm.memcpy.p0.p0.i64({{.*}}i64 48, i1 false)
// CHECK: call void @swift_release
// CHECK: ret ptr
