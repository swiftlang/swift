// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -enable-experimental-feature ImportCStructsWithArcFields %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ImportCStructsWithArcFields

import Foundation
import objc_structs

// IRGen should emit objc_retain for copy and objc_release for destroy
// of strong-only ARC structs, using field-by-field value witnesses.

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCreateAndDestroy
// CHECK: call {{.*}} @"$sSo8MYObjectCABycfC"
// CHECK: call void @llvm.objc.release
// CHECK: ret void
func testCreateAndDestroy() {
  let s = StrongsInAStructArc(myobj: MYObject())
  _ = s
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testCopy
// CHECK: call ptr @llvm.objc.retain
// CHECK: ret ptr
func testCopy(_ s: StrongsInAStructArc) -> StrongsInAStructArc {
  return s
}

// CHECK-LABEL: define {{.*}} @"$s{{.*}}testFieldAccess
// CHECK: call ptr @llvm.objc.retain
// CHECK: ret ptr
func testFieldAccess(_ s: StrongsInAStructArc) -> MYObject {
  return s.myobj
}

// A C function returning a non-trivial ARC struct produces an owned, loadable
// SIL result. IRGen still follows Clang's physical calling convention for the
// aggregate and must not apply the ObjC autoreleased-object return convention.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testReceiveFromCFunction
// CHECK: call {{.*}} @returnStrongArcStruct
// CHECK-NOT: @llvm.objc.retainAutoreleasedReturnValue
// CHECK: ret ptr
func testReceiveFromCFunction() -> MYObject {
  return returnStrongArcStruct().myobj
}

// Two fields fit in the register-return ABI on 64-bit Darwin and reproduce the
// IRGen invalid-cast assertion from swiftlang/swift#93309.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testReceiveStrongPairFromCFunction
// CHECK-NOT: sret
// CHECK: call {{.*}} @returnStrongPairArc
// CHECK-NOT: @llvm.objc.retainAutoreleasedReturnValue
// CHECK: call void @llvm.objc.release
// CHECK: ret void
func testReceiveStrongPairFromCFunction() {
  let pair = returnStrongPairArc()
  _ = pair.first
  _ = pair.second
}

// A larger record remains loadable with a direct SIL result, while Clang's
// target ABI physically returns it through an sret pointer.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testReceiveLargeStrongStructFromCFunction
// CHECK: call void @returnLargeStrongArcStruct
// CHECK-SAME: sret
// CHECK-NOT: @llvm.objc.retainAutoreleasedReturnValue
// CHECK: call void @llvm.objc.release
// CHECK: ret void
func testReceiveLargeStrongStructFromCFunction() {
  let value = returnLargeStrongArcStruct()
  _ = value.first
  _ = value.second
  _ = value.third
}

// Nested records follow the same owned aggregate return path.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testReceiveNestedStrongStructFromCFunction
// CHECK-NOT: sret
// CHECK: call {{.*}} @returnOuterArcStruct
// CHECK-NOT: @llvm.objc.retainAutoreleasedReturnValue
// CHECK: ret void
func testReceiveNestedStrongStructFromCFunction() {
  let value = returnOuterArcStruct()
  _ = value.nested.inner
  _ = value.tag
}

// Imported C function pointers use the same register-return lowering.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testReceiveStrongPairFromFunctionPointer
// CHECK-NOT: sret
// CHECK: call {{.*}} %{{[0-9]+}}()
// CHECK-NOT: @llvm.objc.retainAutoreleasedReturnValue
// CHECK: call void @llvm.objc.release
// CHECK: ret void
func testReceiveStrongPairFromFunctionPointer(_ factory: StrongPairFactory) {
  let pair = factory()
  _ = pair.first
  _ = pair.second
}

// For a large result, an indirect call receives the target ABI's sret pointer
// while the SIL function type still returns an owned, loadable value.
// CHECK-LABEL: define {{.*}} @"$s{{.*}}testReceiveLargeStrongStructFromFunctionPointer
// CHECK: call void %{{[0-9]+}}(
// CHECK-SAME: sret
// CHECK-NOT: @llvm.objc.retainAutoreleasedReturnValue
// CHECK: call void @llvm.objc.release
// CHECK: ret void
func testReceiveLargeStrongStructFromFunctionPointer(_ factory: LargeStrongArcFactory) {
  let value = factory()
  _ = value.first
  _ = value.second
  _ = value.third
}
