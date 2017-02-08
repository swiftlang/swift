// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -module-name OptimizationOptions -Onone -emit-sil -primary-file %s -o - | %FileCheck %s --check-prefix=DEBUG
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -module-name OptimizationOptions -O -emit-sil -primary-file %s -o - | %FileCheck %s --check-prefix=RELEASE
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -module-name OptimizationOptions -Ounchecked -emit-sil -primary-file %s -o - | %FileCheck %s --check-prefix=UNCHECKED
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -module-name OptimizationOptions -Oplayground -emit-sil -primary-file %s -o - | %FileCheck %s --check-prefix=PLAYGROUND

// REQUIRES: optimized_stdlib
// REQUIRES: swift_stdlib_no_asserts

func test_assert(x: Int, y: Int) -> Int {
  assert(x >= y , "x smaller than y")
  return x + y
}

func test_fatal(x: Int, y: Int) -> Int {
  if x > y {
    return x + y
  }
  _preconditionFailure("Human nature ...")
}

func test_precondition_check(x: Int, y: Int) -> Int {
  _precondition(x > y, "test precondition check")
  return x + y
}

func test_partial_safety_check(x: Int, y: Int) -> Int {
  _debugPrecondition(x > y, "test partial safety check")
  return x + y
}

// In debug mode keep user asserts and runtime checks.
// DEBUG-LABEL: sil hidden @_T019OptimizationOptions11test_assertSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// DEBUG: "x smaller than y"
// DEBUG: "assertion failed"
// DEBUG: cond_fail
// DEBUG: return

// In playground mode keep user asserts and runtime checks.
// PLAYGROUND-LABEL: sil hidden @_T019OptimizationOptions11test_assertSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// PLAYGROUND: "x smaller than y"
// PLAYGROUND: "assertion failed"
// PLAYGROUND: cond_fail

// In release mode remove user asserts and keep runtime checks.
// RELEASE-LABEL: sil hidden @_T019OptimizationOptions11test_assertSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// RELEASE-NOT: "x smaller than y"
// RELEASE-NOT: "assertion failed"
// RELEASE: cond_fail

// In fast mode remove user asserts and runtime checks.
// FAST-LABEL: sil hidden @_T019OptimizationOptions11test_assertSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// FAST-NOT: "x smaller than y"
// FAST-NOT: "assertion failed"
// FAST-NOT: cond_fail


// In debug mode keep verbose fatal errors.
// DEBUG-LABEL: sil hidden @_T019OptimizationOptions10test_fatalSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// DEBUG-DAG: "Human nature ..."
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC:.*fatalErrorMessage.*]] : $@convention(thin)
// DEBUG: apply %[[FATAL_ERROR]]({{.*}})
// DEBUG: unreachable

// In playground mode keep verbose fatal errors.
// PLAYGROUND-LABEL: sil hidden @_T019OptimizationOptions10test_fatalSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// PLAYGROUND-DAG: "Human nature ..."
// PLAYGROUND-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC:.*fatalErrorMessage.*]] : $@convention(thin)
// PLAYGROUND: apply %[[FATAL_ERROR]]({{.*}})
// PLAYGROUND: unreachable

// In release mode keep succinct fatal errors (trap).
// RELEASE-LABEL: sil hidden @_T019OptimizationOptions10test_fatalSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// RELEASE-NOT: "Human nature ..."
// RELEASE-NOT: "fatal error"
// RELEASE: cond_fail
// RELEASE: return

// In fast mode remove fatal errors.
// FAST-LABEL: sil hidden @_T019OptimizationOptions10test_fatalSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// FAST-NOT: "Human nature ..."
// FAST-NOT: "fatal error"
// FAST-NOT: int_trap

// Precondition safety checks.

// In debug mode keep verbose library precondition checks.
// DEBUG-LABEL: sil hidden @_T019OptimizationOptions23test_precondition_checkSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// DEBUG-DAG: "fatal error"
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC]]
// DEBUG: apply %[[FATAL_ERROR]]({{.*}})
// DEBUG: unreachable
// DEBUG: return

// In playground mode keep verbose library precondition checks.
// PLAYGROUND-LABEL: sil hidden @_T019OptimizationOptions23test_precondition_checkSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// PLAYGROUND-DAG: "fatal error"
// PLAYGROUND-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC]]
// PLAYGROUND: apply %[[FATAL_ERROR]]({{.*}})
// PLAYGROUND: unreachable
// PLAYGROUND: return

// In release mode keep succinct library precondition checks (trap).
// RELEASE-LABEL: sil hidden @_T019OptimizationOptions23test_precondition_checkSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// RELEASE-NOT:  "fatal error"
// RELEASE:  %[[V2:.+]] = builtin "xor_Int1"(%{{.+}}, %{{.+}})
// RELEASE:  cond_fail %[[V2]]
// RELEASE:  return

// In unchecked mode remove library precondition checks.
// UNCHECKED-LABEL: sil hidden @_T019OptimizationOptions23test_precondition_checkSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// UNCHECKED-NOT:  "fatal error"
// UNCHECKED-NOT:  builtin "int_trap"
// UNCHECKED-NOT:  unreachable
// UNCHECKED:  return

//  Partial safety checks.

// In debug mode keep verbose partial safety checks.
// DEBUG-LABEL: sil hidden @_T019OptimizationOptions25test_partial_safety_checkSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// DEBUG-DAG: "fatal error"
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC]]
// DEBUG: apply %[[FATAL_ERROR]]({{.*}})
// DEBUG: unreachable

// In playground mode keep verbose partial safety checks.
// PLAYGROUND-LABEL: sil hidden @_T019OptimizationOptions25test_partial_safety_checkSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// PLAYGROUND-DAG: "fatal error"
// PLAYGROUND-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC]]
// PLAYGROUND: apply %[[FATAL_ERROR]]({{.*}})
// PLAYGROUND: unreachable

// In release mode remove partial safety checks.
// RELEASE-LABEL: sil hidden @_T019OptimizationOptions25test_partial_safety_checkSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// RELEASE-NOT:  "fatal error"
// RELEASE-NOT:  builtin "int_trap"
// RELEASE-NOT:  unreachable
// RELEASE: return

// In fast mode remove partial safety checks.
// FAST-LABEL: sil hidden @_T019OptimizationOptions25test_partial_safety_checkSiSi1x_Si1ytF : $@convention(thin) (Int, Int) -> Int {
// FAST-NOT:  "fatal error"
// FAST-NOT:  builtin "int_trap"
// FAST-NOT:  unreachable
// FAST: return
