// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.3-abi-triple %s | %FileCheck --check-prefix=CHECK-before %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.4-abi-triple %s | %FileCheck --check-prefix=CHECK-after %s

// REQUIRES: OS=macosx

// Before the runtime provides the support for borrows of values with a
// dynamic layout, IRGen computes that layout from the referent's witnesses.

@available(macOS 27, *)
func foo<Element: ~Copyable>(_ span: borrowing Span<Element>) {
  // CHECK-after: call void @swift_initBorrow

  // CHECK-before-LABEL: define {{.*}} @"$s{{.*}}3foo
  // CHECK-before: init_borrow.value:
  // CHECK-before: call void @llvm.memcpy
  // CHECK-before: init_borrow.pointer:
  _ = Ref(span[0])
}

@available(macOS 27, *)
func bar<Element: ~Copyable>(_ element: borrowing Element,
                            _ body: (borrowing Element) -> Void) {
  // CHECK-after: call ptr @swift_dereferenceBorrow

  // CHECK-before-LABEL: define {{.*}} @"$s{{.*}}3bar
  // CHECK-before: deref_borrow.pointer:
  // CHECK-before: phi ptr
  let r = Ref(element)
  body(r.value)
}

@available(macOS 27, *)
func baz<T: ~Copyable>(_ t: T.Type) -> Int {
  MemoryLayout<Ref<T>>.size
}

// CHECK-before-NOT: swift_getBorrowTypeMetadata
// CHECK-before-NOT: @swift_initBorrow
// CHECK-before-NOT: @swift_dereferenceBorrow

// CHECK-after-DAG: declare void @swift_initBorrow
// CHECK-after-DAG: declare ptr @swift_dereferenceBorrow
