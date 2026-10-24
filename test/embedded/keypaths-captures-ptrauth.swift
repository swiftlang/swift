// Check that a capturing key path, which is copied from a static template into
// a fresh object, re-signs the computed accessor fields for the copy. They're
// signed against their own address (IA key, blended with the KeyPathGetter /
// KeyPathMutatingSetter / KeyPathNonmutatingSetter discriminators), so the
// template's signatures aren't valid in the copy and authenticating them traps.

// RUN: %target-swift-emit-ir %s -module-name main -enable-experimental-feature Embedded -wmo -parse-as-library -O | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: swift_feature_Embedded

public struct Grid {
  public var cells: (Int32, Int32) = (0, 0)
  public subscript(i: Int) -> Int32 {
    get { i == 0 ? cells.0 : cells.1 }
    set { if i == 0 { cells.0 = newValue } else { cells.1 = newValue } }
  }
}

public final class Box {
  public var cells: (Int32, Int32) = (0, 0)
  public subscript(i: Int) -> Int32 {
    get { i == 0 ? cells.0 : cells.1 }
    set { if i == 0 { cells.0 = newValue } else { cells.1 = newValue } }
  }
}

// The getter is at offset 48 and the setter at offset 56: after the object
// header (16), the offset/KVC word (8), the buffer header and skew (8), the
// component header and skew (8), and the component ID (8).

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$e4main6kpGridys15WritableKeyPathCyAA0C0Vs5Int32VGSiF"
// CHECK: [[OBJ:%.*]] = {{.*}}call {{.*}}ptr @swift_allocObject(
// CHECK: call void @llvm.memcpy
// CHECK: [[GETTER_ADDR:%.*]] = getelementptr inbounds {{.*}}i8, ptr [[OBJ]], i64 48
// CHECK: [[GETTER_INT:%.*]] = ptrtoint ptr [[GETTER_ADDR]] to i64
// CHECK: [[GETTER_DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 [[GETTER_INT]], i64 28530)
// CHECK: [[GETTER:%.*]] = {{.*}}call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr @"$e4main4GridVys5Int32VSicipACTK" to i64), i32 0, i64 [[GETTER_DISC]])
// CHECK: [[GETTER_PTR:%.*]] = inttoptr i64 [[GETTER]] to ptr
// CHECK: store ptr [[GETTER_PTR]], ptr [[GETTER_ADDR]]
// CHECK: [[SETTER_ADDR:%.*]] = getelementptr inbounds {{.*}}i8, ptr [[OBJ]], i64 56
// CHECK: [[SETTER_INT:%.*]] = ptrtoint ptr [[SETTER_ADDR]] to i64
// CHECK: [[SETTER_DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 [[SETTER_INT]], i64 29801)
// CHECK: [[SETTER:%.*]] = {{.*}}call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr @"$e4main4GridVys5Int32VSicipACTk" to i64), i32 0, i64 [[SETTER_DISC]])
// CHECK: [[SETTER_PTR:%.*]] = inttoptr i64 [[SETTER]] to ptr
// CHECK: store ptr [[SETTER_PTR]], ptr [[SETTER_ADDR]]
// CHECK: ret ptr [[OBJ]]
public func kpGrid(_ i: Int) -> WritableKeyPath<Grid, Int32> { \Grid[i] }

// A class subscript's setter is nonmutating, which has its own discriminator.

// CHECK-LABEL: define {{.*}}swiftcc ptr @"$e4main5kpBoxys24ReferenceWritableKeyPathCyAA0C0Cs5Int32VGSiF"
// CHECK: [[OBJ:%.*]] = {{.*}}call {{.*}}ptr @swift_allocObject(
// CHECK: call void @llvm.memcpy
// CHECK: [[GETTER_ADDR:%.*]] = getelementptr inbounds {{.*}}i8, ptr [[OBJ]], i64 48
// CHECK: [[GETTER_INT:%.*]] = ptrtoint ptr [[GETTER_ADDR]] to i64
// CHECK: [[GETTER_DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 [[GETTER_INT]], i64 28530)
// CHECK: call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr @"$e4main3BoxCys5Int32VSicipACTK" to i64), i32 0, i64 [[GETTER_DISC]])
// CHECK: [[SETTER_ADDR:%.*]] = getelementptr inbounds {{.*}}i8, ptr [[OBJ]], i64 56
// CHECK: [[SETTER_INT:%.*]] = ptrtoint ptr [[SETTER_ADDR]] to i64
// CHECK: [[SETTER_DISC:%.*]] = {{.*}}call i64 @llvm.ptrauth.blend(i64 [[SETTER_INT]], i64 28528)
// CHECK: call i64 @llvm.ptrauth.sign(i64 ptrtoint (ptr @"$e4main3BoxCys5Int32VSicipACTk" to i64), i32 0, i64 [[SETTER_DISC]])
// CHECK: ret ptr [[OBJ]]
public func kpBox(_ i: Int) -> ReferenceWritableKeyPath<Box, Int32> { \Box[i] }
