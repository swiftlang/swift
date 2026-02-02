// RUN: %target-swift-frontend -typecheck -swift-version 6 %s -verify
// REQUIRES: executable_test
// REQUIRES: OS=wasip1

import WASILibc

// errno is a global thread-local variable, so it should be accessible
// from any context.

enum TestErrno {
  static func testSyncContext() {
    _ = errno
    errno = 0
  }
  static func testAsyncContext() async {
    _ = errno
    errno = 0
  }
}
