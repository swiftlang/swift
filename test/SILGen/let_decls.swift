// RUN: %swift -emit-silgen %s | FileCheck %s

func takeClosure(a : () -> Int) {}

// Let decls don't get boxes for trivial types.
//
// CHECK-LABEL: sil @{{.*}}test1
func test1(a : Int) -> Int {
  // TODO: No box for 'a' either.
  // CHECK: = alloc_box $Int64
  // CHECK: store %0 to 

  // CHECK-NOT: alloc_box
  // CHECK-NOT: alloc_stack

  let (b,c) = (a, 32)

  return b+c
  
  // CHECK: return
}


// Let decls being closed over.
//
// CHECK-LABEL: sil @{{.*}}test2
func test2() {
  // No allocations.
  // CHECK-NOT: alloc_box
  // CHECK-NOT: alloc_stack

  let x = 42

  takeClosure({x})
  
  // CHECK: return
}

// The closure just returns its value, which it captured directly.

// CHECK: sil internal @{{.*}}closure{{.*}} : $@thin (Int64) -> Int64
// CHECK: bb0(%0 : $Int64):
// CHECK:  return %0 : $Int64

