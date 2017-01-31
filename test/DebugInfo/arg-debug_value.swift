// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -primary-file %s -emit-ir -g -o - | %FileCheck %s

// Verify that arguments described by debug_value intrinsics are only
// emitted once.
var g: Int64 = 1

class Foo {
  var x: Int64
  // CHECK: define {{.*}}_T04main3FooCACycfc
  // CHECK: entry:
  // CHECK-NEXT: call void @llvm.dbg.value(metadata %C4main3Foo* %0
  // CHECK: ret %C4main3Foo* %0
  init () { x = g; g += 1 }
}
