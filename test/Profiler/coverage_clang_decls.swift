// RUN: %target-swift-frontend -emit-ir -profile-generate -profile-coverage-mapping -Xcc -fprofile-instr-generate -Xcc -fcoverage-mapping -I %S/Inputs/CModule %s | %FileCheck %s

// Clang's instrumentation is only lowered along with Swift's, so without
// -profile-generate it is dropped instead of reaching the backend unlowered.
// RUN: %target-swift-frontend -emit-object -o /dev/null -Xcc -fprofile-instr-generate -I %S/Inputs/CModule %s 2>&1 | %FileCheck %s --check-prefix=WARN
// RUN: %target-swift-frontend -emit-ir -Xcc -fprofile-instr-generate -I %S/Inputs/CModule %s | %FileCheck %s --check-prefix=NOPROFILE

// WARN: warning: ignoring -Xcc -fprofile-instr-generate (requires -profile-generate)
// WARN-NOT: warning:

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
_ = cfunc()

// CHECK: define {{.*}} @"$So7cstructV$member$setter"
// CHECK: define {{.*}} @"$So7cstructV$member$getter"

// NOPROFILE-NOT: llvm.instrprof
// NOPROFILE: define {{.*}} @cfunc
// NOPROFILE-NOT: llvm.instrprof
