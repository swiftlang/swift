// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-build-swift %s -o %t/a.out
// RUN: not --crash %t/a.out 2>&1 | %utils/backtrace-check

// This is not supported on watchos, ios, or tvos
// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos

// REQUIRES: executable_test

// Backtraces are not emitted when optimizations are enabled. This test can not
// run when optimizations are enabled.
// REQUIRES: swift_test_mode_optimize_none

func main() {
  let x = UnsafePointer<Int>(bitPattern: 0)!
  print("\(x.pointee)")
}

main()
