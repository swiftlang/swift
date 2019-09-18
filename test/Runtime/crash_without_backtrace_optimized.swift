// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O %s -o %t/out
// RUN: %{python} %S/../Inputs/not.py "%target-run %t/out" 2>&1 | %FileCheck --allow-empty %s

// NOTE: not.py is used above instead of "not --crash" because %target-run
// doesn't pass through the crash, and `not` may not be available when running
// on a remote host.

// UNSUPPORTED: OS=watchos
// UNSUPPORTED: OS=ios
// UNSUPPORTED: OS=tvos
// REQUIRES: OS=macosx
// REQUIRES: swift_stdlib_no_asserts
// REQUIRES: executable_test

// This file just causes a crash in the runtime to check whether or not a stack
// trace is produced from the runtime.
//
// It checks that no matter what when we compile with optimization, we do not
// emit backtraces.

// CHECK-NOT: Current stack trace:

import Swift

func foo() -> Int {
  return UnsafePointer<Int>(bitPattern: 0)!.pointee
}

foo()
