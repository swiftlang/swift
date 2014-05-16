// RUN: %swift -Onone -emit-sil %s 2>&1 | FileCheck %s --check-prefix=DEBUG
// RUN: %swift -O0 -emit-sil %s 2>&1 | FileCheck %s --check-prefix=DEBUG
// RUN: %swift -O3 -emit-sil %s 2>&1 | FileCheck %s --check-prefix=RELEASE
// RUN: %swift -O  -emit-sil %s 2>&1 | FileCheck %s --check-prefix=RELEASE
// RUN: %swift -Ofast -emit-sil %s 2>&1 | FileCheck %s --check-prefix=FAST

// REQUIRES: optimized_stdlib

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
// DEBUG: "Human nature ..."
// DEBUG: "fatal error"
// DEBUG: int_trap

// In release mode keep succinct fatal errors (trap).
// RELEASE-LABEL: _TF19OptimizationOptions10test_fatalFTSiSi_Si
// RELEASE-NOT: "Human nature ..."
// RELEASE-NOT: "fatal error"
// RELEASE: int_trap

// In fast mode remove fatal errors.
// FAST-LABEL: _TF19OptimizationOptions10test_fatalFTSiSi_Si
// FAST-NOT: "Human nature ..."
// FAST-NOT: "fatal error"
// FAST-NOT: int_trap

// Precondition safety checks.

// In debug mode keep verbose library precondition checks.
// DEBUG-LABEL: _TF19OptimizationOptions23test_precondition_checkFTSiSi_Si
// DEBUG:  "fatal error"
// DEBUG:  function_ref @swift_reportFatalError
// DEBUG:  builtin_function_ref "int_trap"
// DEBUG:  unreachable
// DEBUG:  return

// In release mode keep succinct library precondition checks (trap).
// RELEASE-LABEL: _TF19OptimizationOptions23test_precondition_checkFTSiSi_Si
// RELEASE-NOT:  "fatal error"
// RELEASE:  builtin_function_ref "int_trap"
// RELEASE:  unreachable
// RELEASE:  return

// In fast mode remove library precondition checks.
// FAST-LABEL: _TF19OptimizationOptions23test_precondition_checkFTSiSi_Si
// FAST-NOT:  "fatal error"
// FAST-NOT:  builtin_function_ref "int_trap"
// FAST-NOT:  unreachable
// FAST:  return

//  Partial safety checks.

// In debug mode keep verbose partial safety checks.
// DEBUG-LABEL: _TF19OptimizationOptions25test_partial_safety_checkFTSiSi_Si
// DEBUG:  "fatal error"
// DEBUG:  function_ref @swift_reportFatalError
// DEBUG:  builtin_function_ref "int_trap"
// DEBUG:  unreachable

// In release mode remove partial safety checks.
// RELEASE-LABEL: _TF19OptimizationOptions25test_partial_safety_checkFTSiSi_Si
// RELEASE-NOT:  "fatal error"
// RELEASE-NOT:  builtin_function_ref "int_trap"
// RELEASE-NOT:  unreachable
// RELEASE: return

// In fast mode remove partial safety checks.
// FAST-LABEL: _TF19OptimizationOptions25test_partial_safety_checkFTSiSi_Si
// FAST-NOT:  "fatal error"
// FAST-NOT:  builtin_function_ref "int_trap"
// FAST-NOT:  unreachable
// FAST: return
