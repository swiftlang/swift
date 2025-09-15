// RUN: %target-swift-frontend -emit-ir -profile-generate -profile-coverage-mapping -Xcc -fprofile-instr-generate -Xcc -fcoverage-mapping -I %S/Inputs/CModule %s | %FileCheck %s

// https://github.com/apple/swift/issues/57483 (rdar://82820628)
// Make sure we don't crash in IRGen attempting to emit the coverage map for any
// implicit Clang decls.

import CModule

func testStruct(_ x: UnsafePointer<cstruct>?) {
  _ = x?.pointee.member
  _ = cstruct()
  _ = cstruct(member: 5)
}
testStruct(nil)

// CHECK: define {{.*}} @"$So7cstructV$member$setter"
// CHECK: define {{.*}} @"$So7cstructV$member$getter"
