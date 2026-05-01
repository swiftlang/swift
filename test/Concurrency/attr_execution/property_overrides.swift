// RUN: %target-swift-emit-silgen %s -target %target-swift-5.1-abi-triple -language-mode 6 -enable-upcoming-feature NonisolatedNonsendingByDefault | %FileCheck %s

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

class Base {
  // CHECK: Base.prop.getter
  // CHECK: Isolation: nonisolated(nonsending)
  // CHECK-LABEL: sil hidden [ossa] @$s18property_overrides4BaseC4propSivg : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed Base) -> (Int, @error any Error)
  var prop: Int { get async throws { 0 } }
}

class OverrideWithSync: Base {
  // CHECK: OverrideWithSync.prop.getter
  // CHECK: Isolation: nonisolated
  // CHECK-LABEL: sil hidden [ossa] @$s18property_overrides16OverrideWithSyncC4propSivg : $@convention(method) (@guaranteed OverrideWithSync) -> Int
  override var prop: Int { 42 }
}

// CHECK: // vtable thunk for Base.prop.getter dispatching to OverrideWithSync.prop.getter
// CHECK: sil private [thunk] [ossa] @$s18property_overrides16OverrideWithSyncC4propSivgAA4BaseCADSivgTV : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed OverrideWithSync) -> (Int, @error any Error) {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, [[SELF:%.*]] : @guaranteed $OverrideWithSync):
// CHECK:   [[PROP_OVERRIDE:%.*]] = function_ref @$s18property_overrides16OverrideWithSyncC4propSivg : $@convention(method) (@guaranteed OverrideWithSync) -> Int
// CHECK:   [[RESULT:%.*]] = apply [[PROP_OVERRIDE]]([[SELF]]) : $@convention(method) (@guaranteed OverrideWithSync) -> Int
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s18property_overrides16OverrideWithSyncC4propSivgAA4BaseCADSivgTV'

class OverridenWithAsync: Base {
  // CHECK: OverridenWithAsync.prop.getter
  // CHECK: Isolation: nonisolated(nonsending)
  // CHECK-LABEL: sil hidden [ossa] @$s18property_overrides18OverridenWithAsyncC4propSivg : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed OverridenWithAsync) -> Int
  override var prop: Int { get async { 42 } }
}

// CHECK: // vtable thunk for Base.prop.getter dispatching to OverridenWithAsync.prop.getter
// CHECK: sil private [thunk] [ossa] @$s18property_overrides18OverridenWithAsyncC4propSivgAA4BaseCADSivgTV : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed OverridenWithAsync) -> (Int, @error any Error) {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Builtin.ImplicitActor, [[SELF:%.*]] : @guaranteed $OverridenWithAsync):
// CHECK:   [[PROP_OVERRIDE:%.*]] = function_ref @$s18property_overrides18OverridenWithAsyncC4propSivg : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed OverridenWithAsync) -> Int
// CHECK:   [[RESULT:%.*]] = apply [[PROP_OVERRIDE]]([[ACTOR]], [[SELF]]) : $@convention(method) @caller_isolated @async (@sil_isolated @sil_implicit_leading_param @guaranteed Builtin.ImplicitActor, @guaranteed OverridenWithAsync) -> Int
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   return [[RESULT]]
// CHECK: } // end sil function '$s18property_overrides18OverridenWithAsyncC4propSivgAA4BaseCADSivgTV'
