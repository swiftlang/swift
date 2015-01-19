// RUN: %target-swift-frontend -module-name OptimizationOptions -disable-func-sig-opts -Onone -emit-sil -primary-file %s 2>&1 | FileCheck %s --check-prefix=DEBUG
// RUN: %target-swift-frontend -module-name OptimizationOptions -disable-func-sig-opts -O -emit-sil -primary-file %s 2>&1 | FileCheck %s --check-prefix=RELEASE
// RUN: %target-swift-frontend -module-name OptimizationOptions -disable-func-sig-opts -Ounchecked -emit-sil -primary-file %s 2>&1 | FileCheck %s --check-prefix=UNCHECKED

// REQUIRES: optimized_stdlib
// REQUIRES: swift_stdlib_asserts
// REQUIRES: swift_stdlib_no_asserts

func test_assert() (x: Int, y: Int) -> Int {
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
// DEBUG-LABEL: _TF19OptimizationOptions11test_assertfT_FT1xSi1ySi_Si
// DEBUG: "x smaller than y"
// DEBUG: "assertion failed"
// DEBUG: cond_fail

// In release mode remove user asserts and keep runtime checks.
// RELEASE-LABEL: _TF19OptimizationOptions11test_assertfT_FT1xSi1ySi_Si
// RELEASE-NOT: "x smaller than y"
// RELEASE-NOT: "assertion failed"
// RELEASE: cond_fail

// In fast mode remove user asserts and runtime checks.
// FAST-LABEL: _TF19OptimizationOptions11test_assertfT_FT1xSi1ySi_Si
// FAST-NOT: "x smaller than y"
// FAST-NOT: "assertion failed"
// FAST-NOT: cond_fail


// In debug mode keep verbose fatal errors.
// DEBUG-LABEL: _TF19OptimizationOptions10test_fatalFTSiSi_Si
// DEBUG-DAG: "Human nature ..."
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @_TTOS_nndd__TFSs18_fatalErrorMessageFTVSs12StaticStringS_S_Su_T_
// DEBUG: apply %[[FATAL_ERROR]]{{.*}} @noreturn
// DEBUG: unreachable

// In release mode keep succinct fatal errors (trap).
// RELEASE-LABEL: _TF19OptimizationOptions10test_fatalFTSiSi_Si
// RELEASE-NOT: "Human nature ..."
// RELEASE-NOT: "fatal error"
// RELEASE: bb2:
// RELEASE: %[[ALWAYS:.+]] = integer_literal $Builtin.Int1, -1
// RELEASE: cond_fail %[[ALWAYS]]

// In fast mode remove fatal errors.
// FAST-LABEL: _TF19OptimizationOptions10test_fatalFTSiSi_Si
// FAST-NOT: "Human nature ..."
// FAST-NOT: "fatal error"
// FAST-NOT: int_trap

// Precondition safety checks.

// In debug mode keep verbose library precondition checks.
// DEBUG-LABEL: _TF19OptimizationOptions23test_precondition_checkFTSiSi_Si
// DEBUG-DAG: "fatal error"
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @_TTOS_nndd__TFSs18_fatalErrorMessageFTVSs12StaticStringS_S_Su_T_
// DEBUG: apply %[[FATAL_ERROR]]{{.*}} @noreturn
// DEBUG: unreachable
// DEBUG: return

// In release mode keep succinct library precondition checks (trap).
// RELEASE-LABEL: _TF19OptimizationOptions23test_precondition_checkFTSiSi_Si
// RELEASE-NOT:  "fatal error"
// RELEASE:  %[[V2:.+]] = builtin "xor_Int1"(%{{.+}}, %{{.+}})
// RELEASE:  cond_fail %[[V2]]
// RELEASE:  return

// In unchecked mode remove library precondition checks.
// UNCHECKED-LABEL: _TF19OptimizationOptions23test_precondition_checkFTSiSi_Si
// UNCHECKED-NOT:  "fatal error"
// UNCHECKED-NOT:  builtin "int_trap"
// UNCHECKED-NOT:  unreachable
// UNCHECKED:  return

//  Partial safety checks.

// In debug mode keep verbose partial safety checks.
// DEBUG-LABEL: _TF19OptimizationOptions25test_partial_safety_checkFTSiSi_Si
// DEBUG-DAG: "fatal error"
// DEBUG-DAG: %[[FATAL_ERROR:.+]] = function_ref @_TTOS_nndd__TFSs18_fatalErrorMessageFTVSs12StaticStringS_S_Su_T_
// DEBUG: apply %[[FATAL_ERROR]]{{.*}} @noreturn
// DEBUG: unreachable

// In release mode remove partial safety checks.
// RELEASE-LABEL: _TF19OptimizationOptions25test_partial_safety_checkFTSiSi_Si
// RELEASE-NOT:  "fatal error"
// RELEASE-NOT:  builtin "int_trap"
// RELEASE-NOT:  unreachable
// RELEASE: return

// In fast mode remove partial safety checks.
// FAST-LABEL: _TF19OptimizationOptions25test_partial_safety_checkFTSiSi_Si
// FAST-NOT:  "fatal error"
// FAST-NOT:  builtin "int_trap"
// FAST-NOT:  unreachable
// FAST: return
