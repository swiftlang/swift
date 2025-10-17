// RUN: %target-swift-frontend -parse-as-library -emit-ir -disable-llvm-merge-functions-pass %s | %FileCheck --check-prefix=NO-TBI %s
// RUN: %target-swift-frontend -parse-as-library -Xllvm -aarch64-use-tbi -emit-ir -disable-llvm-merge-functions-pass %s | %FileCheck --check-prefix=TBI %s

// This test makes sure that we can properly fold the mask for the witness table
// when we have a #isolation.

// PLEASE READ THIS! On 6.2, we do not eliminate hops for
// nonisolated(nonsending) to be conservative. So we have to pattern match
// differently than on main!

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
// TBI: define internal swifttailcc void @"$s18isolation_macro_ir46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaFTY0_"(ptr swiftasync [[ASYNC_CONTEXT:%.*]])
// TBI: [[ASYNC_FRAME_PTR:%.*]] = getelementptr inbounds i8, ptr [[ASYNC_CONTEXT]], i32 16
// TBI: [[SECOND_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 3
// TBI: [[SECOND_WORD:%.*]] = load i64, ptr [[SECOND_WORD_ADDR]]
// TBI: [[FIRST_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 2
// TBI: [[FIRST_WORD:%.*]] = load i64, ptr [[FIRST_WORD_ADDR]]
// TBI: [[MASKED_SECOND_WORD:%.*]] = and i64 [[SECOND_WORD]], -3458764513820540929
// TBI: call swiftcc void @"$s18isolation_macro_ir8useActor3isoyScA_pSg_tF"(i64 [[FIRST_WORD]], i64 [[MASKED_SECOND_WORD]])

// NO-TBI: define internal swifttailcc void @"$s18isolation_macro_ir46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaFTY0_"(ptr swiftasync [[ASYNC_CONTEXT:%.*]])
// NO-TBI: [[ASYNC_FRAME_PTR:%.*]] = getelementptr inbounds i8, ptr [[ASYNC_CONTEXT]], i32 16
// NO-TBI: [[SECOND_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 3
// NO-TBI: [[SECOND_WORD:%.*]] = load i64, ptr [[SECOND_WORD_ADDR]]
// NO-TBI: [[FIRST_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 2
// NO-TBI: [[FIRST_WORD:%.*]] = load i64, ptr [[FIRST_WORD_ADDR]]
// NO-TBI: [[MASKED_SECOND_WORD:%.*]] = and i64 [[SECOND_WORD]], -4
// NO-TBI: call swiftcc void @"$s18isolation_macro_ir8useActor3isoyScA_pSg_tF"(i64 [[FIRST_WORD]], i64 [[MASKED_SECOND_WORD]])

public nonisolated(nonsending) func nonisolatedNonsendingUsePoundIsolationDirectly() async {
  let iso = #isolation
  useActor(iso: iso)
}

// #isolation via default arg
//
// TBI: define internal swifttailcc void @"$s18isolation_macro_ir45nonisolatedNonsendingPoundIsolationDefaultArgyyYaFTY0_"(ptr swiftasync [[ASYNC_CONTEXT:%.*]])
// TBI: [[ASYNC_FRAME_PTR:%.*]] = getelementptr inbounds i8, ptr [[ASYNC_CONTEXT]], i32 16
// TBI: [[SECOND_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 2
// TBI: [[SECOND_WORD:%.*]] = load i64, ptr [[SECOND_WORD_ADDR]]
// TBI: [[FIRST_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 1
// TBI: [[FIRST_WORD:%.*]] = load i64, ptr [[FIRST_WORD_ADDR]]
// TBI: [[MASKED_SECOND_WORD:%.*]] = and i64 [[SECOND_WORD]], -3458764513820540929
// TBI: call swiftcc void @"$s18isolation_macro_ir13implicitParamyyScA_pSgF"(i64 [[FIRST_WORD]], i64 [[MASKED_SECOND_WORD]])

// NO-TBI: define internal swifttailcc void @"$s18isolation_macro_ir45nonisolatedNonsendingPoundIsolationDefaultArgyyYaFTY0_"(ptr swiftasync [[ASYNC_CONTEXT:%.*]])
// NO-TBI: [[ASYNC_FRAME_PTR:%.*]] = getelementptr inbounds i8, ptr [[ASYNC_CONTEXT]], i32 16
// NO-TBI: [[SECOND_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 2
// NO-TBI: [[SECOND_WORD:%.*]] = load i64, ptr [[SECOND_WORD_ADDR]]
// NO-TBI: [[FIRST_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 1
// NO-TBI: [[FIRST_WORD:%.*]] = load i64, ptr [[FIRST_WORD_ADDR]]
// NO-TBI: [[MASKED_SECOND_WORD:%.*]] = and i64 [[SECOND_WORD]], -4
// NO-TBI: call swiftcc void @"$s18isolation_macro_ir13implicitParamyyScA_pSgF"(i64 [[FIRST_WORD]], i64 [[MASKED_SECOND_WORD]])
public nonisolated(nonsending) func nonisolatedNonsendingPoundIsolationDefaultArg() async {
  implicitParam()
}

@inline(never)
public nonisolated(nonsending) func calleeFunction() async {
}

// TBI: define internal swifttailcc void @"$s18isolation_macro_ir14callerFunctionyyYaFTY0_"(ptr swiftasync [[ASYNC_CONTEXT:%.*]])
// TBI: [[ASYNC_FRAME_PTR:%.*]] = getelementptr inbounds i8, ptr [[ASYNC_CONTEXT]], i32 16
// TBI: [[SECOND_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 2
// TBI: [[SECOND_WORD:%.*]] = load i64, ptr [[SECOND_WORD_ADDR]]
// TBI: [[MASKED_SECOND_WORD:%.*]] = and i64 [[SECOND_WORD]], -3458764513820540929
// TBI: [[FIRST_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 1
// TBI: [[FIRST_WORD:%.*]] = load i64, ptr [[FIRST_WORD_ADDR]]
// TBI: musttail call swifttailcc void @"$s18isolation_macro_ir14calleeFunctionyyYaF"(ptr swiftasync {{%.*}}, i64 [[FIRST_WORD]], i64 [[MASKED_SECOND_WORD]])

// NO-TBI: define internal swifttailcc void @"$s18isolation_macro_ir14callerFunctionyyYaFTY0_"(ptr swiftasync [[ASYNC_CONTEXT:%.*]])
// NO-TBI: [[ASYNC_FRAME_PTR:%.*]] = getelementptr inbounds i8, ptr [[ASYNC_CONTEXT]], i32 16
// NO-TBI: [[SECOND_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 2
// NO-TBI: [[SECOND_WORD:%.*]] = load i64, ptr [[SECOND_WORD_ADDR]]
// NO-TBI: [[MASKED_SECOND_WORD:%.*]] = and i64 [[SECOND_WORD]], -4
// NO-TBI: [[FIRST_WORD_ADDR:%.*]] = getelementptr inbounds %"{{.*}}.Frame", ptr [[ASYNC_FRAME_PTR]], i32 0, i32 1
// NO-TBI: [[FIRST_WORD:%.*]] = load i64, ptr [[FIRST_WORD_ADDR]]
// NO-TBI: musttail call swifttailcc void @"$s18isolation_macro_ir14calleeFunctionyyYaF"(ptr swiftasync {{%.*}}, i64 [[FIRST_WORD]], i64 [[MASKED_SECOND_WORD]])
@inline(never)
public nonisolated(nonsending) func callerFunction() async {
  await calleeFunction()
}
