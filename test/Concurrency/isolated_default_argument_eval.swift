// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -I %t  -target %target-swift-5.1-abi-triple -strict-concurrency=complete -enable-upcoming-feature IsolatedDefaultValues -parse-as-library %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_IsolatedDefaultValues

@MainActor
func requiresMainActor() -> Int { 0 }

// CHECK-LABEL: sil hidden [ossa] @$s30isolated_default_argument_eval19mainActorDefaultArg5valueS2i_tFfA_
@discardableResult
@MainActor
func mainActorDefaultArg(value: Int = requiresMainActor()) -> Int {
  value
}

@MainActor
func mainActorMultiDefaultArg(x: Int = requiresMainActor(),
                              y: Int = 0,
                              tuple: (Int, Int) = (requiresMainActor(), 2),
                              z: Int = 0) {}

// CHECK-LABEL: sil hidden [ossa] @$s30isolated_default_argument_eval15mainActorCalleryyF
@MainActor func mainActorCaller() {
  mainActorDefaultArg()
  mainActorMultiDefaultArg()
}

// CHECK-LABEL: sil hidden [ossa] @$s30isolated_default_argument_eval22nonisolatedAsyncCalleryyYaF
func nonisolatedAsyncCaller() async {
  // CHECK: hop_to_executor {{.*}} : $Optional<Builtin.Executor>
  // CHECK: hop_to_executor {{.*}} : $MainActor
  // CHECK: [[GET_VALUE:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval19mainActorDefaultArg5valueS2i_tFfA_
  // CHECK-NEXT: apply [[GET_VALUE]]()
  // CHECK: hop_to_executor {{.*}} : $Optional<Builtin.Executor>
  await mainActorDefaultArg()

  // CHECK: hop_to_executor {{.*}} : $MainActor
  // CHECK: [[GET_X:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval24mainActorMultiDefaultArg1x1y5tuple1zySi_S2i_SitSitFfA_
  // CHECK-NEXT: apply [[GET_X]]()
  // CHECK-NOT: hop_to_executor
  // CHECK: [[GET_Y:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval24mainActorMultiDefaultArg1x1y5tuple1zySi_S2i_SitSitFfA0_
  // CHECK-NEXT: apply [[GET_Y]]()
  // CHECK-NOT: hop_to_executor
  // CHECK: [[GET_TUPLE:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval24mainActorMultiDefaultArg1x1y5tuple1zySi_S2i_SitSitFfA1_
  // CHECK-NEXT: apply [[GET_TUPLE]]()
  // CHECK-NOT: hop_to_executor
  // CHECK: [[GET_Z:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval24mainActorMultiDefaultArg1x1y5tuple1zySi_S2i_SitSitFfA2_
  // CHECK-NEXT: apply [[GET_Z]]()
  // CHECK: hop_to_executor {{.*}} : $Optional<Builtin.Executor>
  await mainActorMultiDefaultArg()
}

@MainActor
func isolatedDefaultInoutMix(x: inout Int, y: Int, z: Int = requiresMainActor()) {}

var argValue: Int { 0 }

// CHECK-LABEL: sil hidden [ossa] @$s30isolated_default_argument_eval20passInoutWithDefaultyyYaF
func passInoutWithDefault() async {
  // CHECK: hop_to_executor {{.*}} : $Optional<Builtin.Executor>

  var x = 0

  // CHECK: [[GET_ARG_VALUE:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval8argValueSivg
  // CHECK-NEXT: [[ARG_VALUE:%[0-9]+]] = apply [[GET_ARG_VALUE]]()
  // CHECK-NEXT: [[INOUT_X:%[0-9]+]] = begin_access [modify]
  // CHECK: hop_to_executor {{.*}} : $MainActor
  // CHECK: [[GET_Z:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval0A15DefaultInoutMix1x1y1zySiz_S2itFfA1_
  // CHECK-NEXT: [[Z:%[0-9]+]] = apply [[GET_Z]]()
  // CHECK: [[FN:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval0A15DefaultInoutMix1x1y1zySiz_S2itF : $@convention(thin) (@inout Int, Int, Int) -> ()
  // CHECK: apply [[FN]]([[INOUT_X]], [[ARG_VALUE]], [[Z]])
  // CHECK: hop_to_executor {{.*}} : $Optional<Builtin.Executor>
  await isolatedDefaultInoutMix(x: &x, y: argValue)
}

// default argument 0 of noSuspensionInDefaultArgGenerator(_:)
// CHECK-LABEL: sil hidden [ossa] @$s30isolated_default_argument_eval33noSuspensionInDefaultArgGeneratoryySiFfA_
// CHECK: [[NESTED_DEFAULT_REF:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval19mainActorDefaultArg5valueS2i_tFfA_ : $@convention(thin) () -> Int
// CHECK-NEXT: [[NESTED_DEFAULT:%[0-9]+]] = apply [[NESTED_DEFAULT_REF]]() : $@convention(thin) () -> Int
// CHECK: [[DEFAULT_REF:%[0-9]+]] = function_ref @$s30isolated_default_argument_eval19mainActorDefaultArg5valueS2i_tF : $@convention(thin) (Int) -> Int
// CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[DEFAULT_REF]]([[NESTED_DEFAULT]]) : $@convention(thin) (Int) -> Int
// CHECK-NEXT: return [[RESULT]] : $Int

// CHECK-LABEL: sil hidden [ossa] @$s30isolated_default_argument_eval33noSuspensionInDefaultArgGeneratoryySiF
@MainActor func noSuspensionInDefaultArgGenerator(
  _ x: Int = mainActorDefaultArg()
) {}

func testNoSuspensionInDefaultArgGenerator() async {
  await noSuspensionInDefaultArgGenerator()
}
