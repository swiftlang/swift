// RUN: %empty-directory(%t)

// RUN: %target-build-swift %s -o %t/a.out -O -target %target-future-triple
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

struct S {
  var x = 42
}

_ = [S() as Sendable]
