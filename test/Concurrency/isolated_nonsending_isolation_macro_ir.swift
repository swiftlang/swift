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
// TBI: define swifttailcc void @"$s38isolated_nonsending_isolation_macro_ir46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaF"(ptr swiftasync %0, ptr %1, ptr [[PTR:%.*]])
// TBI: [[ALLOCA_1:%.*]] = alloca %swift.implicit_isolated_actor_type
// TBI: [[ALLOCA_2:%.*]] = alloca <{ i64, i64 }>
// TBI: [[ALLOCA_1_WITNESS_POINTER:%.*]] = getelementptr inbounds %swift.implicit_isolated_actor_type, ptr [[ALLOCA_1]], i32 0, i32 1
// TBI: store ptr [[PTR]], ptr [[ALLOCA_1_WITNESS_POINTER]]
// TBI: [[ALLOCA_1_WITNESS_POINTER:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[ALLOCA_1]], i32 0, i32 1
// TBI: [[RELOAD_PTR:%.*]] = load i64, ptr [[ALLOCA_1_WITNESS_POINTER]]
// TBI: [[MASKED_PTR:%.*]] = and i64 [[RELOAD_PTR]], -3458764513820540929
// TBI: [[ALLOCA_2_WITNESS_POINTER:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[ALLOCA_2]], i32 0, i32 1
// TBI: store i64 [[MASKED_PTR]], ptr [[ALLOCA_2_WITNESS_POINTER]]
// TBI: [[ALLOCA_2_WITNESS_POINTER:%.*]] = getelementptr inbounds { i64, i64 }, ptr [[ALLOCA_2]], i32 0, i32 1
// TBI: [[RELOAD_MASKED_PTR:%.*]] = load i64, ptr [[ALLOCA_2_WITNESS_POINTER]]
// TBI: call swiftcc void @"$s38isolated_nonsending_isolation_macro_ir8useActor3isoyScA_pSg_tF"(i64 {{%.*}}, i64 [[RELOAD_MASKED_PTR]])

// NO-TBI: define swifttailcc void @"$s38isolated_nonsending_isolation_macro_ir46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaF"(ptr swiftasync %0, ptr %1, ptr [[PTR:%.*]])
// NO-TBI: [[ALLOCA_1:%.*]] = alloca %swift.implicit_isolated_actor_type
// NO-TBI: [[ALLOCA_2:%.*]] = alloca <{ i64, i64 }>
// NO-TBI: [[ALLOCA_1_WITNESS_POINTER:%.*]] = getelementptr inbounds %swift.implicit_isolated_actor_type, ptr [[ALLOCA_1]], i32 0, i32 1
// NO-TBI: store ptr [[PTR]], ptr [[ALLOCA_1_WITNESS_POINTER]]
// NO-TBI: [[ALLOCA_1_WITNESS_POINTER:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[ALLOCA_1]], i32 0, i32 1
// NO-TBI: [[RELOAD_PTR:%.*]] = load i64, ptr [[ALLOCA_1_WITNESS_POINTER]]
// NO-TBI: [[MASKED_PTR:%.*]] = and i64 [[RELOAD_PTR]], -4
// NO-TBI: [[ALLOCA_2_WITNESS_POINTER:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[ALLOCA_2]], i32 0, i32 1
// NO-TBI: store i64 [[MASKED_PTR]], ptr [[ALLOCA_2_WITNESS_POINTER]]
// NO-TBI: [[ALLOCA_2_WITNESS_POINTER:%.*]] = getelementptr inbounds { i64, i64 }, ptr [[ALLOCA_2]], i32 0, i32 1
// NO-TBI: [[RELOAD_MASKED_PTR:%.*]] = load i64, ptr [[ALLOCA_2_WITNESS_POINTER]]
// NO-TBI: call swiftcc void @"$s38isolated_nonsending_isolation_macro_ir8useActor3isoyScA_pSg_tF"(i64 {{%.*}}, i64 [[RELOAD_MASKED_PTR]])
public nonisolated(nonsending) func nonisolatedNonsendingUsePoundIsolationDirectly() async {
  let iso = #isolation
  useActor(iso: iso)
}

// #isolation via default arg
//
// TBI: define swifttailcc void @"$s38isolated_nonsending_isolation_macro_ir45nonisolatedNonsendingPoundIsolationDefaultArgyyYaF"(ptr swiftasync {{%.*}}, ptr {{%.*}}, ptr [[PTR:%.*]])
// TBI: [[ALLOCA_1:%.*]] = alloca %swift.implicit_isolated_actor_type
// TBI: [[ALLOCA_2:%.*]] = alloca <{ i64, i64 }>
// TBI: [[ALLOCA_1_WITNESS_POINTER:%.*]] = getelementptr inbounds %swift.implicit_isolated_actor_type, ptr [[ALLOCA_1]], i32 0, i32 1
// TBI: store ptr [[PTR]], ptr [[ALLOCA_1_WITNESS_POINTER]]
// TBI: [[ALLOCA_1_WITNESS_POINTER:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[ALLOCA_1]], i32 0, i32 1
// TBI: [[RELOAD_PTR:%.*]] = load i64, ptr [[ALLOCA_1_WITNESS_POINTER]]
// TBI: [[MASKED_PTR:%.*]] = and i64 [[RELOAD_PTR]], -3458764513820540929
// TBI: [[ALLOCA_2_WITNESS_POINTER:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[ALLOCA_2]], i32 0, i32 1
// TBI: store i64 [[MASKED_PTR]], ptr [[ALLOCA_2_WITNESS_POINTER]]
// TBI: [[ALLOCA_2_WITNESS_POINTER:%.*]] = getelementptr inbounds { i64, i64 }, ptr [[ALLOCA_2]], i32 0, i32 1
// TBI: [[RELOAD_MASKED_PTR:%.*]] = load i64, ptr [[ALLOCA_2_WITNESS_POINTER]]
// TBI: call swiftcc void @"$s38isolated_nonsending_isolation_macro_ir13implicitParamyyScA_pSgF"(i64 {{%.*}}, i64 [[RELOAD_MASKED_PTR]])

// NO-TBI: define swifttailcc void @"$s38isolated_nonsending_isolation_macro_ir45nonisolatedNonsendingPoundIsolationDefaultArgyyYaF"(ptr swiftasync {{%.*}}, ptr {{%.*}}, ptr [[PTR:%.*]])
// NO-TBI: [[ALLOCA_1:%.*]] = alloca %swift.implicit_isolated_actor_type
// NO-TBI: [[ALLOCA_2:%.*]] = alloca <{ i64, i64 }>
// NO-TBI: [[ALLOCA_1_WITNESS_POINTER:%.*]] = getelementptr inbounds %swift.implicit_isolated_actor_type, ptr [[ALLOCA_1]], i32 0, i32 1
// NO-TBI: store ptr [[PTR]], ptr [[ALLOCA_1_WITNESS_POINTER]]
// NO-TBI: [[ALLOCA_1_WITNESS_POINTER:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[ALLOCA_1]], i32 0, i32 1
// NO-TBI: [[RELOAD_PTR:%.*]] = load i64, ptr [[ALLOCA_1_WITNESS_POINTER]]
// NO-TBI: [[MASKED_PTR:%.*]] = and i64 [[RELOAD_PTR]], -4
// NO-TBI: [[ALLOCA_2_WITNESS_POINTER:%.*]] = getelementptr inbounds <{ i64, i64 }>, ptr [[ALLOCA_2]], i32 0, i32 1
// NO-TBI: store i64 [[MASKED_PTR]], ptr [[ALLOCA_2_WITNESS_POINTER]]
// NO-TBI: [[ALLOCA_2_WITNESS_POINTER:%.*]] = getelementptr inbounds { i64, i64 }, ptr [[ALLOCA_2]], i32 0, i32 1
// NO-TBI: [[RELOAD_MASKED_PTR:%.*]] = load i64, ptr [[ALLOCA_2_WITNESS_POINTER]]
// NO-TBI: call swiftcc void @"$s38isolated_nonsending_isolation_macro_ir13implicitParamyyScA_pSgF"(i64 {{%.*}}, i64 [[RELOAD_MASKED_PTR]])
public nonisolated(nonsending) func nonisolatedNonsendingPoundIsolationDefaultArg() async {
  implicitParam()
}

@inline(never)
public nonisolated(nonsending) func calleeFunction() async {
}

// TBI: define swifttailcc void @"$s38isolated_nonsending_isolation_macro_ir14callerFunctionyyYaF"(ptr swiftasync %0, ptr %1, ptr [[PTR:%.*]])
// TBI: [[PTR_INT:%.*]] = ptrtoint ptr [[PTR]] to i64
// TBI: [[MASKED_PTR_INT:%.*]] = and i64 [[PTR_INT]], -3458764513820540929
// TBI: [[MASKED_PTR:%.*]] = inttoptr i64 [[MASKED_PTR_INT]] to ptr
// TBI: musttail call swifttailcc void @"$s38isolated_nonsending_isolation_macro_ir14calleeFunctionyyYaF"(ptr swiftasync {{%.*}}, ptr {{%.*}}, ptr [[MASKED_PTR]])

// NO-TBI: define swifttailcc void @"$s38isolated_nonsending_isolation_macro_ir14callerFunctionyyYaF"(ptr swiftasync {{%.*}}, ptr {{%.*}}, ptr [[PTR:%.*]])
// NO-TBI: [[PTR_INT:%.*]] = ptrtoint ptr [[PTR]] to i64
// NO-TBI: [[MASKED_PTR_INT:%.*]] = and i64 [[PTR_INT]], -4
// NO-TBI: [[MASKED_PTR:%.*]] = inttoptr i64 [[MASKED_PTR_INT]] to ptr
// NO-TBI: musttail call swifttailcc void @"$s38isolated_nonsending_isolation_macro_ir14calleeFunctionyyYaF"(ptr swiftasync {{%.*}}, ptr {{%.*}}, ptr [[MASKED_PTR]])
@inline(never)
public nonisolated(nonsending) func callerFunction() async {
  await calleeFunction()
}
