// RUN: %target-swift-frontend -module-name OptimizationOptions -Onone -emit-sil -primary-file %s -o - | %FileCheck %s --check-prefix=DEBUG
// RUN: %target-swift-frontend -module-name OptimizationOptions -O -emit-sil -primary-file %s -o - | %FileCheck %s --check-prefix=RELEASE
// RUN: %target-swift-frontend -module-name OptimizationOptions -Ounchecked -emit-sil -primary-file %s -o - | %FileCheck %s --check-prefix=UNCHECKED
// RUN: %target-swift-frontend -module-name OptimizationOptions -Oplayground -emit-sil -primary-file %s -o - | %FileCheck %s --check-prefix=PLAYGROUND

// REQUIRES: optimized_stdlib
// REQUIRES: swift_stdlib_no_asserts

// SWIFT_ENABLE_TENSORFLOW
// Optimizations are currently enabled for -Oplayground, causing the original
// PLAYGROUND checks to fail.
// Until optimizations are no longer needed for deabstraction/partitioning, the
// PLAYGROUND checks will temporarily be changed to match the RELEASE checks.

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
  _precondition(x > y, "Test precondition check")
  return x + y
}

func test_partial_safety_check(x: Int, y: Int) -> Int {
  _debugPrecondition(x > y, "Test partial safety check")
  return x + y
}

// In debug mode keep user asserts and runtime checks.
// DEBUG-LABEL: sil hidden @$S19OptimizationOptions11test_assert1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// DEBUG: "x smaller than y"
// DEBUG: "Assertion failed"
// DEBUG: cond_fail
// DEBUG: return

// SWIFT_ENABLE_TENSORFLOW: temporarily change PLAYGROUND checks to match RELEASE checks.
// In playground mode keep user asserts and runtime checks.
// PLAYGROUND-LABEL: sil hidden @$S19OptimizationOptions11test_assert1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// PLAYGROUND-NOT: "x smaller than y"
// PLAYGROUND-NOT: "Assertion failed"
// PLAYGROUND: cond_fail

// In release mode remove user asserts and keep runtime checks.
// RELEASE-LABEL: sil hidden @$S19OptimizationOptions11test_assert1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// RELEASE-NOT: "x smaller than y"
// RELEASE-NOT: "Assertion failed"
// RELEASE: cond_fail

// In fast mode remove user asserts and runtime checks.
// FAST-LABEL: sil hidden @$S19OptimizationOptions11test_assert1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// FAST-NOT: "x smaller than y"
// FAST-NOT: "Assertion failed"
// FAST-NOT: cond_fail


// In debug mode keep verbose fatal errors.
// DEBUG-LABEL: sil hidden @$S19OptimizationOptions10test_fatal1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// DEBUG-DAG: "Human nature ..."
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC:.*fatalErrorMessage.*]] : $@convention(thin)
// DEBUG: apply %[[FATAL_ERROR]]({{.*}})
// DEBUG: unreachable

// SWIFT_ENABLE_TENSORFLOW: temporarily change PLAYGROUND checks to match RELEASE checks.
// In playground mode keep succinct fatal errors (trap).
// PLAYGROUND-LABEL: sil hidden @$S19OptimizationOptions10test_fatal1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// PLAYGROUND-NOT: "Human nature ..."
// PLAYGROUND-NOT: "Fatal error"
// PLAYGROUND: cond_fail
// PLAYGROUND: return

// In release mode keep succinct fatal errors (trap).
// RELEASE-LABEL: sil hidden @$S19OptimizationOptions10test_fatal1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// RELEASE-NOT: "Human nature ..."
// RELEASE-NOT: "Fatal error"
// RELEASE: cond_fail
// RELEASE: return

// In fast mode remove fatal errors.
// FAST-LABEL: sil hidden @$S19OptimizationOptions10test_fatal1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// FAST-NOT: "Human nature ..."
// FAST-NOT: "Fatal error"
// FAST-NOT: int_trap

// Precondition safety checks.

// In debug mode keep verbose library precondition checks.
// DEBUG-LABEL: sil hidden @$S19OptimizationOptions23test_precondition_check1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// DEBUG-DAG: "Fatal error"
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC]]
// DEBUG: apply %[[FATAL_ERROR]]({{.*}})
// DEBUG: unreachable
// DEBUG: return

// SWIFT_ENABLE_TENSORFLOW: temporarily change PLAYGROUND checks to match RELEASE checks.
// In playground mode keep succinct library precondition checks (trap).
// PLAYGROUND-LABEL: sil hidden @$S19OptimizationOptions23test_precondition_check1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// PLAYGROUND-NOT:  "Fatal error"
// PLAYGROUND:  %[[V2:.+]] = builtin "xor_Int1"(%{{.+}}, %{{.+}})
// PLAYGROUND:  cond_fail %[[V2]]
// PLAYGROUND:  return

// In release mode keep succinct library precondition checks (trap).
// RELEASE-LABEL: sil hidden @$S19OptimizationOptions23test_precondition_check1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// RELEASE-NOT:  "Fatal error"
// RELEASE:  %[[V2:.+]] = builtin "xor_Int1"(%{{.+}}, %{{.+}})
// RELEASE:  cond_fail %[[V2]]
// RELEASE:  return

// In unchecked mode remove library precondition checks.
// UNCHECKED-LABEL: sil hidden @$S19OptimizationOptions23test_precondition_check1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// UNCHECKED-NOT:  "Fatal error"
// UNCHECKED-NOT:  builtin "int_trap"
// UNCHECKED-NOT:  unreachable
// UNCHECKED:  return

//  Partial safety checks.

// In debug mode keep verbose partial safety checks.
// DEBUG-LABEL: sil hidden @$S19OptimizationOptions25test_partial_safety_check1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// DEBUG-DAG: "Fatal error"
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @[[FATAL_ERROR_FUNC]]
// DEBUG: apply %[[FATAL_ERROR]]({{.*}})
// DEBUG: unreachable

// SWIFT_ENABLE_TENSORFLOW: temporarily change PLAYGROUND checks to match RELEASE checks.
// In playground mode remove partial safety checks.
// PLAYGROUND-LABEL: sil hidden @$S19OptimizationOptions25test_partial_safety_check1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// PLAYGROUND-NOT:  "Fatal error"
// PLAYGROUND-NOT:  builtin "int_trap"
// PLAYGROUND-NOT:  unreachable
// PLAYGROUND: return

// In release mode remove partial safety checks.
// RELEASE-LABEL: sil hidden @$S19OptimizationOptions25test_partial_safety_check1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// RELEASE-NOT:  "Fatal error"
// RELEASE-NOT:  builtin "int_trap"
// RELEASE-NOT:  unreachable
// RELEASE: return

// In fast mode remove partial safety checks.
// FAST-LABEL: sil hidden @$S19OptimizationOptions25test_partial_safety_check1x1yS2i_SitF : $@convention(thin) (Int, Int) -> Int {
// FAST-NOT:  "Fatal error"
// FAST-NOT:  builtin "int_trap"
// FAST-NOT:  unreachable
// FAST: return
