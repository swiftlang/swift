// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -o %t/a.out
// RUN: env SWIFT_BACKTRACE=enable=no %{python} %S/../Inputs/not.py "%target-run %t/a.out" 2>&1 | %{python} %utils/backtrace-check

// NOTE: not.py is used above instead of "not --crash" because %target-run
// doesn't pass through the crash, and `not` may not be available when running
// on a remote host.

// This is not supported on watchos, ios, or tvos
// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
// UNSUPPORTED: OS=linux-android, OS=linux-androideabi

// REQUIRES: swift_stdlib_asserts
// REQUIRES: executable_test

// Backtraces are not emitted when optimizations are enabled. This test can not
// run when optimizations are enabled.
// REQUIRES: swift_test_mode_optimize_none

// This file just causes a crash in the runtime to check whether or not a stack
// trace is produced from the runtime.

func main() {
  let x = UnsafePointer<Int>(bitPattern: 0)!
  print("\(x.pointee)")
}

main()
