// RUN: %target-swift-frontend -parse-as-library -Xllvm -aarch64-use-tbi -emit-ir -disable-llvm-merge-functions-pass %s | %FileCheck %s

// This test makes sure that we can properly fold the mask for the witness table
// when we have a #isolation.

// REQUIRES: concurrency
// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: PTRSIZE=64
// REQUIRES: OS=macosx || OS=ios
// REQUIRES: CPU=arm64

@inline(never)
func useActor(iso: (any Actor)?) {
  print(iso!.unownedExecutor)
}

@inline(never)
func implicitParam(_ x: (any Actor)? = #isolation) {
  print(x!.unownedExecutor)
}

// #isolation via direct usage
//
// CHECK-LABEL: define internal swifttailcc void @"$s41isolated_nonsending_isolation_macro_ir_6446nonisolatedNonsendingUsePoundIsolationDirectlyyyYaFTY0_"(
// CHECK: [[MASK:%.*]] = and i64 {{%.*}}, -3458764513820540929
// CHECK: [[MEM_ADJUSTED:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[MEM:%.*]], i32 0, i32 1
// CHECK: store i64 [[MASK]], ptr [[MEM_ADJUSTED]]
// CHECK: [[MEM_ADJUSTED_2:%.*]] = getelementptr inbounds { i64, i64 }, ptr [[MEM]], i32 0, i32 1
// CHECK: [[MEM_LOAD:%.*]] = load i64, ptr [[MEM_ADJUSTED_2]]
// CHECK:   call swiftcc void @"$s41isolated_nonsending_isolation_macro_ir_648useActor3isoyScA_pSg_tF"(i64 {{%.*}}, i64 [[MEM_LOAD]])
public nonisolated(nonsending) func nonisolatedNonsendingUsePoundIsolationDirectly() async {
  let iso = #isolation
  useActor(iso: iso)
}

// #isolation via default arg
//
// CHECK-LABEL: define internal swifttailcc void @"$s41isolated_nonsending_isolation_macro_ir_6445nonisolatedNonsendingPoundIsolationDefaultArgyyYaFTY0_"(
// CHECK: [[MASK:%.*]] = and i64 {{%.*}}, -3458764513820540929
// CHECK: [[MEM_ADJUSTED:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[MEM:%.*]], i32 0, i32 1
// CHECK: store i64 [[MASK]], ptr [[MEM_ADJUSTED]]
// CHECK: [[MEM_ADJUSTED_2:%.*]] = getelementptr inbounds { i64, i64 }, ptr [[MEM]], i32 0, i32 1
// CHECK: [[MEM_LOAD:%.*]] = load i64, ptr [[MEM_ADJUSTED_2]]
// CHECK: call swiftcc void @"$s41isolated_nonsending_isolation_macro_ir_6413implicitParamyyScA_pSgF"(i64 {{%.*}}, i64 [[MEM_LOAD]])
public nonisolated(nonsending) func nonisolatedNonsendingPoundIsolationDefaultArg() async {
  implicitParam()
}
