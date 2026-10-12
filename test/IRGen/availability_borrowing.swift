// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.3-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-before %s
// RUN: %target-swift-frontend -emit-ir -target %target-swift-6.4-abi-triple %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-after %s

// REQUIRES: OS=macosx

@available(macOS 27, *)
func foo<Element: ~Copyable>(_ span: borrowing Span<Element>) {
  // CHECK: call void @swift_initBorrow
  _ = Ref(span[0])
}


// CHECK-before: declare extern_weak void @swift_initBorrow
// CHECK-after: declare void @swift_initBorrow
