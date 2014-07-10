// RUN: rm -rf %t
// RUN: %swift -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 -sdk %S/Inputs -O -emit-sil -I %S/Inputs -enable-source-import %s -enable-union-import | FileCheck %s

import gizmo

// CHECK: ModifyStruct
// CHECK: %1 = alloc_stack $Drill
// CHECK: ret
func ModifyStruct(inDrill : Drill) -> Int32 {
  var D : Drill = inDrill;
  D.x += 3
  return D.x;
}
