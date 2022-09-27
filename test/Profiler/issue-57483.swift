// RUN: %target-swift-frontend -emit-ir -profile-generate -profile-coverage-mapping -Xcc -fprofile-instr-generate -Xcc -fcoverage-mapping -I %S/Inputs/issue-57483 %s | %FileCheck %s

// https://github.com/apple/swift/issues/57483 (rdar://82820628)
// Make sure we don't crash in IRGen attempting to emit the coverage map for the
// implicit Clang getter for the member bitfield.

// CHECK: define {{.*}} @"$So7cstructV$member$getter"

import CModule

func foo(_ x: UnsafePointer<cstruct>?) {
  _ = x?.pointee.member
}
foo(nil)
