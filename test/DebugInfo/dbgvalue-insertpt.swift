// RUN: %target-swift-frontend -g -emit-ir -Xllvm '-sil-inline-never-functions=next' %s | %FileCheck %s

// FIXME: This test should be testing a non-shadow-copied value instead.
for i in 0 ..< 3 {
  // CHECK: %[[ALLOCA:[0-9]+]] = alloca %TSiSg
  // CHECK: %i.debug = alloca i{{32|64}}
  // CHECK-NEXT: #dbg_declare(ptr %i.debug,
  // CHECK-SAME:                           ![[I:[0-9]+]],
  // CHECK: ![[I]] = !DILocalVariable(name: "i",
}
