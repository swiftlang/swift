// RUN: %target-swift-frontend -emit-sil -sil-verify-all %s | %FileCheck %s

// When ClosureLifetimeFixup promotes a closure to the stack, we should
// eliminate the temporary copy of any borrowed move-only captures, so that
// they get checked as borrows and not consumes.

struct E<T>: ~Copyable {
    var t: T
}

class Klass {}
struct C<T> {
  var t: T
}

var escaper: () -> () = {}

func nonescaping(_ x: () -> ()) { }
func escaping(_ x: @escaping () -> ()) {  escaper = x }

func borrow<T>(_: borrowing E<T>) {}
func borrow<T>(_: borrowing C<T>) {}
func mutate<T>(_: inout C<T>) {}

// CHECK-LABEL: sil {{.*}}14testCopySafetyyyAA1CVyxGlF : 
// CHECK: [[STK1:%.*]] = alloc_stack {{.*}} $C<T>
// CHECK: copy_addr %0 to [init] [[STK1]]
// CHECK: partial_apply {{.*}}[on_stack] {{.*}}([[STK1]]) : $@convention(thin) <τ_0_0> (@inout_aliasable C<τ_0_0>) -> ()
// CHECK: [[STK2:%.*]] = alloc_stack {{.*}} $C<T>
// CHECK: [[BA:%.*]] = begin_access [read] [static] [[STK1]]
// CHECK: copy_addr [[BA]] to [init] [[STK2]]
// CHECK: end_access [[BA]]
// CHECK: partial_apply {{.*}}[on_stack] {{.*}}([[STK2]]) : $@convention(thin) <τ_0_0> (@in_guaranteed C<τ_0_0>) -> ()
func testCopySafety<T>(_ c: C<T>) {
  var mutC = c
  nonescaping({ mutate(&mutC) }, and: {[mutC] in borrow(mutC) })
}

// CHECK-LABEL: sil {{.*}}18testVarReadCaptureyyAA1CVyxGlF :
// CHECK: [[STK:%.*]] = alloc_stack {{.*}} $C<T>
// CHECK: copy_addr %0 to [init] [[STK]]
// CHECK: partial_apply {{.*}}[on_stack] {{.*}}([[STK]]) : $@convention(thin) <τ_0_0> (@inout_aliasable C<τ_0_0>) -> ()
func testVarReadCapture<T>(_ c: C<T>) {
  var mutC = c
  nonescaping {
    borrow(mutC)
  }
}

// CHECK-LABEL: sil {{.*}}16testNeverEscaped
func testNeverEscaped<T>(_ e: borrowing E<T>) {
    // CHECK: partial_apply {{.*}}[on_stack] {{.*}}(%0) :
    nonescaping {
        borrow(e)
    }
}

func nonescaping(_ x: () -> (), and y: () -> ()) {}

// CHECK-LABEL: sil {{.*}}21testNeverEscapedTwice
func testNeverEscapedTwice<T>(_ e: borrowing E<T>) {
    // CHECK: partial_apply {{.*}}[on_stack] {{.*}}(%0) :
    // CHECK: partial_apply {{.*}}[on_stack] {{.*}}(%0) :
    nonescaping({ borrow(e) }, and: { borrow(e) })
}

// CHECK-LABEL: sil {{.*}}16testEscapedLater
func testEscapedLater<T>(_ e: consuming E<T>) {
    // CHECK: [[BOX:%.*]] = alloc_box
    // CHECK: [[CONTENTS:%.*]] = project_box [[BOX]]
    // CHECK: partial_apply {{.*}}[on_stack] {{.*}}([[CONTENTS]])
    nonescaping {
        borrow(e)
    }

    escaping {
        borrow(e)
    }
}

// CHECK-LABEL: sil {{.*}}17testEscapedLater2
func testEscapedLater2<T>(_ e: consuming E<T>) {
    // CHECK: [[BOX:%.*]] = alloc_box
    // CHECK: [[CONTENTS:%.*]] = project_box [[BOX]]
    // CHECK: partial_apply {{.*}}[on_stack] {{.*}}([[CONTENTS]])
    let f = e

    nonescaping {
        borrow(f)
    }

    escaping {
        borrow(f)
    }
}

// Copyable borrowing parameters use move-only wrappers while checking ownership.
// Removing the capture copy must also work through those wrappers.

// CHECK-LABEL: sil {{.*}}testCopyableNeverEscaped
// CHECK-NOT: copy_addr
// CHECK: partial_apply {{.*}}[on_stack] {{.*}}(%0) :
// CHECK-NOT: copy_addr
// CHECK: return
func testCopyableNeverEscaped<T>(_ c: borrowing C<T>) {
  nonescaping {
    borrow(c)
  }
}

// CHECK-LABEL: sil {{.*}}testCopyableNestedCapture
// CHECK-NOT: copy_addr
// CHECK: partial_apply {{.*}}[on_stack] {{.*}}(%0) :
// CHECK-NOT: copy_addr
// CHECK: return
func testCopyableNestedCapture<T>(_ c: borrowing C<T>) {
  nonescaping {
    nonescaping {
      borrow(c)
    }
  }
}

func mayThrow() throws {}

func throwingNonescaping(_ body: () throws -> ()) rethrows {
  try body()
}

// CHECK-LABEL: sil {{.*}}testCopyableThrowingCapture
// CHECK-NOT: copy_addr
// CHECK: partial_apply {{.*}}[on_stack] {{.*}}(%0) :
// CHECK-NOT: copy_addr
// CHECK: try_apply
func testCopyableThrowingCapture<T>(_ c: borrowing C<T>) throws {
  try throwingNonescaping {
    borrow(c)
    try throwingNonescaping {
      borrow(c)
      try mayThrow()
    }
  }
}
