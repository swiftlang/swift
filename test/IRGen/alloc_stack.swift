// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

class Foobar {
  init() {
    var a : Bool = true
  }
}

// Make sure we are mis-initializing the alloca.
// CHECK-LABEL: define {{.*}}swiftcc %T11alloc_stack6FoobarC* @"$s11alloc_stack6FoobarCACycfc"(%T11alloc_stack6FoobarC* swiftself)
// CHECK: alloca %TSb, align 1
// CHECK-NOT: store{{.*}}opaque
// CHECK:  ret {{.*}}%0
// CHECK:}

