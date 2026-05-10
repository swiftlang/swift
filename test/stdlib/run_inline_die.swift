// RUN: %target-build-swift -parse-as-library -Xfrontend -disable-availability-checking -module-name a %s -o %t.out
// RUN: %target-codesign %t.out
// RUN: %target-run %t.out

// REQUIRES: executable_test
// REQUIRES: freestanding
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// This test should always exit with a non-zero code.
// XFAIL: *

@_spi(_TaskToThreadModel) import _Concurrency

@main struct Main {
  static func main() async {
    // Check that we crash when calling runInline from within an async context--
    // here the async context of an async @main.
    Task.runInline {}
  }
}
