// Tests for the Swift frontends `-verify` mode.

// Turn off "pipefail" mode because we intend the `swift` invocation to
// terminate with a non-zero exit code, and we don't want the test to fail
// because of this.
// RUN: set +o pipefail && %target-typecheck-verify-swift 2>&1 | %FileCheck %s

// CHECK: [[@LINE+1]]:1: error: unexpected error produced: use of unresolved
undefinedFunc()

// CHECK: [[@LINE+1]]:4: error: expected error not produced
// expected-error{{This error message never gets output}}

// CHECK: [[@LINE+1]]:20: error: expected {{{{}} in {{expected}}-{{warning}}
// expected-warning
