// RUN: %swift -O0 -emit-sil %s 2>&1 | FileCheck %s --check-prefix=DEBUG
// RUN: %swift -O3 -emit-sil %s 2>&1 | FileCheck %s --check-prefix=RELEASE
// RUN: %swift -OFast -emit-sil %s 2>&1 | FileCheck %s --check-prefix=FAST

func test_assert() (x: Int, y: Int) -> Int {
  assert(x >= y , "x smaller than y")
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

// In release mode remove user asserts and runtime checks.
// FAST-LABEL: _TF19OptimizationOptions11test_assertfT_FT1xSi1ySi_Si
// FAST-NOT: "x smaller than y"
// FAST-NOT: "assertion failed"
// FAST-NOT: cond_fail
