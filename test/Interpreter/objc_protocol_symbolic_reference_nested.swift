// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx14 %s -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// Match the macosx14 target above.
// REQUIRES: stdlib_5_9_runtime

import Foundation

struct S<T> {}

struct Outer {
  @objc protocol InnerP {}
}

let t = S<Outer.InnerP>.self
print(t, ObjectIdentifier(t))

// CHECK: S<InnerP> ObjectIdentifier(0x
