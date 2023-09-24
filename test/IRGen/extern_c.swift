// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

func test() {
  // CHECK: call void @explicit_extern_c()
  explicit_extern_c()
  // CHECK: call void @"$s8extern_c09implicit_A2_cyyF"()
  implicit_extern_c()
}

test()

// CHECK: declare void @explicit_extern_c()
@_extern(c, "explicit_extern_c") func explicit_extern_c()

// CHECK: declare void @"$s8extern_c09implicit_A2_cyyF"()
@_extern(c) func implicit_extern_c()
