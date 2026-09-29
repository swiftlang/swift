// RUN: %target-build-swift -Xfrontend -enable-builtin-module -Xfrontend -sil-verify-all %s -o %t
// RUN: %target-codesign %t
// RUN: %target-run %t | %FileCheck %s
// RUN: %target-build-swift -O -Xfrontend -enable-builtin-module -Xfrontend -sil-verify-all %s -o %t-opt
// RUN: %target-codesign %t-opt
// RUN: %target-run %t-opt | %FileCheck %s
// REQUIRES: executable_test

import Builtin

var destructions = 0
final class Object {
  let value = 42
  deinit { destructions += 1 }
}

@inline(never)
func create() -> UnsafeMutableRawPointer {
  Unmanaged.passRetained(Object()).toOpaque()
}

@inline(never)
func take(_ pointer: UnsafeMutableRawPointer) -> Object {
  Builtin.takeFromRawPointer(pointer._rawValue)
}

@inline(never)
func exercise() {
  let pointer = create()
  precondition(destructions == 0)
  let object = take(pointer)
  precondition(Unmanaged.passUnretained(object).toOpaque() == pointer)
  precondition(object.value == 42)
  withExtendedLifetime(object) { precondition(destructions == 0) }
}
exercise()
precondition(destructions == 1)
print("adoption balanced")
// CHECK: adoption balanced
