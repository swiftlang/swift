// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -primary-file %s -emit-ir -g -o - | %FileCheck %s

// Capture the pointer size from type Int
// CHECK: %TSi = type <{ i[[PTRSIZE:[0-9]+]] }>

struct A {
  var fn : () -> ()
}

func test(_ x : A) {
  var _ = x
}
// CHECK:    define hidden {{.*}}void @_T07structs4test{{[_0-9a-zA-Z]*}}F
// CHECK: [[X_DBG:%.*]] = alloca
// CHECK: call void @llvm.dbg.declare(metadata {{.*}}* [[X_DBG]], metadata [[X_MD:!.*]], metadata
// CHECK: ![[A_DI:.*]] = !DICompositeType(tag: DW_TAG_structure_type, name: "A",{{.*}}identifier

class C {
  var lots_of_extra_storage: (Int, Int, Int) = (1, 2, 3)
  var member: C = C()
}

// CHECK: [[X_MD]] = !DILocalVariable(name: "x", arg: 1
// CHECK-SAME:                         type: ![[A_DI]]

// A class is represented by a pointer, so B's total size should be PTRSIZE.
// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "B",{{.*}}size: [[PTRSIZE]]
struct B {
  var c : C
}
