// RUN: %target-swift-emit-silgen %s  | %FileCheck -check-prefix CHECK -check-prefix DISABLED %s
// RUN: %target-swift-emit-silgen %s -enable-upcoming-feature NonisolatedNonsendingByDefault | %FileCheck -check-prefix CHECK -check-prefix ENABLED %s

// REQUIRES: concurrency
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// Validate that both with and without the experimental flag we properly codegen
// `@concurrent` and `nonisolated(nonsending)`

// CHECK-LABEL: // executionCaller()
// CHECK-NEXT: // Isolation: caller_isolation_inheriting
// CHECK-NEXT: sil hidden [ossa] @$s14execution_attr0A6CalleryyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
nonisolated(nonsending)
func executionCaller() async {}

// CHECK-LABEL: // executionConcurrent()
// CHECK: // Isolation: nonisolated
// CHECK: sil hidden [ossa] @$s14execution_attr0A10ConcurrentyyYaF : $@convention(thin) @async () -> () {
@concurrent
func executionConcurrent() async {}

// DISABLED: sil hidden [ossa] @$s14execution_attr0A15CallerParameteryyyyYaYCXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// ENABLED: sil hidden [ossa] @$s14execution_attr0A15CallerParameteryyyyYaYCXEYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> ()) -> () {
// CHECK: } // end sil function '$s14execution_attr0A15CallerParameteryyyyYaYCXEYaF'
func executionCallerParameter(_ x: nonisolated(nonsending) () async -> ()) async {
  await x()
}

// DISABLED-LABEL: sil hidden [ossa] @$s14execution_attr0A19ConcurrentParameteryyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
// ENABLED-LABEL: sil hidden [ossa] @$s14execution_attr0A19ConcurrentParameteryyyyYaXEYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
func executionConcurrentParameter(_ x: @concurrent () async -> ()) async {
  await x()
}

struct S {
  let field: nonisolated(nonsending) () async -> ()
}

// DISABLED: sil hidden [ossa] @$s14execution_attr0A11CallerFieldyyAA1SVYaF : $@convention(thin) @async (@guaranteed S) -> () {
// DISABLED: bb0([[ARG:%.*]] : @guaranteed $S):
// DISABLED:   [[FIELD:%.*]] = struct_extract [[ARG]]
// DISABLED:   [[FIELD_COPY:%.*]] = copy_value [[FIELD]]
// DISABLED:   [[ACTOR_NONE:%.*]] = enum $Optional<any Actor>, #Optional.none!enumelt
// DISABLED:   [[BORROWED_FIELD:%.*]] = begin_borrow [[FIELD_COPY]]
// DISABLED:   apply [[BORROWED_FIELD]]([[ACTOR_NONE]])
// DISABLED: } // end sil function '$s14execution_attr0A11CallerFieldyyAA1SVYaF'

// ENABLED: sil hidden [ossa] @$s14execution_attr0A11CallerFieldyyAA1SVYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed S) -> () {
// ENABLED: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>, [[ARG:%.*]] : @guaranteed $S):
// ENABLED:   [[FIELD:%.*]] = struct_extract [[ARG]]
// ENABLED:   [[FIELD_COPY:%.*]] = copy_value [[FIELD]]
// ENABLED:   [[BORROWED_FIELD:%.*]] = begin_borrow [[FIELD_COPY]]
// ENABLED:   apply [[BORROWED_FIELD]]([[ACTOR]])
// ENABLED: } // end sil function '$s14execution_attr0A11CallerFieldyyAA1SVYaF'
func executionCallerField(_ s: S) async {
  await s.field()
}

extension S {
  // CHECK-LABEL: // S.executionCallerFieldMethod(_:)
  // CHECK: // Isolation: unspecified
  // CHECK: sil hidden [ossa] @$s14execution_attr1SV0A17CallerFieldMethodyyyyYaYCXEF : $@convention(method) (@guaranteed @noescape @async @callee_guaranteed (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> (), @guaranteed S) -> () {
  func executionCallerFieldMethod(_ x: nonisolated(nonsending) () async -> ()) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s14execution_attr24testWithDynamicIsolation2fnyyyYAXE_tYaF : $@convention(thin) @async (@guaranteed @isolated(any) @noescape @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[PARAM_FN:%.*]] : @guaranteed $@isolated(any) @noescape @callee_guaranteed () -> ()):
// CHECK:   [[GENERIC_EXEC:%.*]] = enum $Optional<Builtin.Executor>, #Optional.none!enumelt
// CHECK-NEXT:   hop_to_executor [[GENERIC_EXEC]]
// CHECK-NEXT:   [[FN:%.*]] = copy_value [[PARAM_FN]]
// CHECK-NEXT:   [[BORROWED_FN:%.*]] = begin_borrow [[FN]]
// CHECK-NEXT:   [[FN_ISOLATION:%.*]] = function_extract_isolation [[BORROWED_FN]]
// CHECK-NEXT:   hop_to_executor [[FN_ISOLATION]]
// CHECK-NEXT:   [[BORROWED_FN:%.*]] = begin_borrow [[FN]]
// CHECK-NEXT:   apply [[BORROWED_FN]]()
// CHECK: hop_to_executor [[GENERIC_EXEC]]
// CHECK: } // end sil function '$s14execution_attr24testWithDynamicIsolation2fnyyyYAXE_tYaF'
@concurrent
func testWithDynamicIsolation(fn: @isolated(any) () -> Void) async {
  await fn()
}

// CHECK-LABEL: sil hidden [ossa] @$s14execution_attr38testCallerIsolatedWithDynamicIsolation2fnyyyYAXE_tYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>, @guaranteed @isolated(any) @noescape @callee_guaranteed () -> ()) -> () {
// CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[PARAM_FN:%.*]] : @guaranteed $@isolated(any) @noescape @callee_guaranteed () -> ()):
// CHECK:   hop_to_executor [[ISOLATION]]
// CHECK-NEXT:   [[FN:%.*]] = copy_value [[PARAM_FN]]
// CHECK-NEXT:   [[BORROWED_FN:%.*]] = begin_borrow [[FN]]
// CHECK-NEXT:   [[FN_ISOLATION:%.*]] = function_extract_isolation [[BORROWED_FN]]
// CHECK-NEXT:   hop_to_executor [[FN_ISOLATION]]
// CHECK-NEXT:   [[BORROWED_FN:%.*]] = begin_borrow [[FN]]
// CHECK-NEXT:   apply [[BORROWED_FN]]()
// CHECK: hop_to_executor [[ISOLATION]]
// CHECK: } // end sil function '$s14execution_attr38testCallerIsolatedWithDynamicIsolation2fnyyyYAXE_tYaF'
nonisolated(nonsending)
func testCallerIsolatedWithDynamicIsolation(fn: @isolated(any) () -> Void) async {
  await fn()
}
