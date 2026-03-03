// RUN: %target-swift-frontend -parse-as-library -emit-ir -disable-llvm-merge-functions-pass %s | %FileCheck --check-prefix=NO-TBI %s
// RUN: %target-swift-frontend -parse-as-library -Xllvm -aarch64-use-tbi -emit-ir -disable-llvm-merge-functions-pass %s | %FileCheck --check-prefix=TBI %s

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
// TBI: define swifttailcc void @"$s18isolation_macro_ir46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaF"(ptr swiftasync %0, i64 %1, i64 [[SECOND_WORD:%.*]])
// TBI: [[MASKED_SECOND_WORD:%.*]] = and i64 [[SECOND_WORD]], -3458764513820540929
// TBI: call swiftcc void @"$s18isolation_macro_ir8useActor3isoyScA_pSg_tF"(i64 %1, i64 [[MASKED_SECOND_WORD]])

// NO-TBI: define swifttailcc void @"$s18isolation_macro_ir46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaF"(ptr swiftasync %0, i64 [[FIRST_WORD:%.*]], i64 [[SECOND_WORD:%.*]])
// NO-TBI: [[MASKED_SECOND_WORD:%.*]] = and i64 [[SECOND_WORD]], -4
// NO-TBI: call swiftcc void @"$s18isolation_macro_ir8useActor3isoyScA_pSg_tF"(i64 [[FIRST_WORD]], i64 [[MASKED_SECOND_WORD]])

public nonisolated(nonsending) func nonisolatedNonsendingUsePoundIsolationDirectly() async {
  let iso = #isolation
  useActor(iso: iso)
}

// #isolation via default arg
//
// TBI: define swifttailcc void @"$s18isolation_macro_ir45nonisolatedNonsendingPoundIsolationDefaultArgyyYaF"(ptr swiftasync {{%.*}}, i64 {{%.*}}, i64 [[WORD_2:%.*]])
// TBI: [[MASKED_WORD_2:%.*]] = and i64 [[WORD_2]], -3458764513820540929
// TBI: call swiftcc void @"$s18isolation_macro_ir13implicitParamyyScA_pSgF"(i64 {{%.*}}, i64 [[MASKED_WORD_2]])

// NO-TBI: define swifttailcc void @"$s18isolation_macro_ir45nonisolatedNonsendingPoundIsolationDefaultArgyyYaF"(ptr swiftasync %0, i64 [[WORD_1:%.*]], i64 [[WORD_2:%.*]])
// NO-TBI: [[MASKED_WORD_2:%.*]] = and i64 [[WORD_2]], -4
// NO-TBI: call swiftcc void @"$s18isolation_macro_ir13implicitParamyyScA_pSgF"(i64 {{%.*}}, i64 [[MASKED_WORD_2]])
public nonisolated(nonsending) func nonisolatedNonsendingPoundIsolationDefaultArg() async {
  implicitParam()
}

@inline(never)
public nonisolated(nonsending) func calleeFunction() async {
}

// TBI: define swifttailcc void @"$s18isolation_macro_ir14callerFunctionyyYaF"(ptr swiftasync %0, i64 %1, i64 [[WORD_2:%.*]])
// TBI: [[MASKED_WORD_2:%.*]] = and i64 [[WORD_2]], -3458764513820540929
// TBI: musttail call swifttailcc void @"$s18isolation_macro_ir14calleeFunctionyyYaF"(ptr swiftasync {{%.*}}, i64 {{%.*}}, i64 [[MASKED_WORD_2]])

// NO-TBI: define swifttailcc void @"$s18isolation_macro_ir14callerFunctionyyYaF"(ptr swiftasync %0, i64 %1, i64 [[WORD:%.*]])
// NO-TBI: [[MASKED_WORD:%.*]] = and i64 [[WORD]], -4
// NO-TBI: musttail call swifttailcc void @"$s18isolation_macro_ir14calleeFunctionyyYaF"(ptr swiftasync {{%.*}}, i64 {{%.*}}, i64 [[MASKED_WORD]])
@inline(never)
public nonisolated(nonsending) func callerFunction() async {
  await calleeFunction()
}
