// RUN: %target-swift-frontend %use_no_opaque_pointers -g -emit-ir -Xllvm '-sil-inline-never-functions=next' %s | %FileCheck %s
// RUN: %target-swift-frontend -g -emit-ir -Xllvm '-sil-inline-never-functions=next' %s

// FIXME: This test should be testing a non-shadow-copied value instead.
for i in 0 ..< 3 {
  // CHECK: %[[ALLOCA:[0-9]+]] = alloca %TSiSg
  // CHECK: %i.debug = alloca i{{32|64}}
  // CHECK-NEXT: call void @llvm.dbg.declare(metadata i{{32|64}}* %i.debug,
  // CHECK-SAME:                           metadata ![[I:[0-9]+]],
  // CHECK: ![[I]] = !DILocalVariable(name: "i",
}
