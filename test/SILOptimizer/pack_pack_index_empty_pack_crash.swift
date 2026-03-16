// RUN: %target-swift-frontend -O -emit-sil -sil-verify-all %s | %FileCheck %s

// Verify that generic specialization of a variadic generic function called
// with an empty parameter pack does not crash.

@inline(never)
func variadicFunc<A, B, each C>(_ a: A, _ b: B, _ c: repeat each C) -> (A, B, repeat each C) {
  return (a, b, repeat each c)
}

// Verify that the specialized function for <Int, String, Pack{}> does not
// contain any pack indexing instructions — all pack operations must be
// eliminated when the variadic parameter pack is empty.

// CHECK-LABEL: sil {{.*}}@$s{{.*}}12variadicFunc{{.*}} :
// CHECK-NOT: pack_pack_index
// CHECK-NOT: dynamic_pack_index
// CHECK-NOT: open_pack_element
// CHECK-NOT: pack_element_get
// CHECK-NOT: pack_element_set
// CHECK: } // end sil function

// CHECK-LABEL: sil @$s{{.*}}4test
// CHECK-NOT: pack_pack_index
// CHECK-NOT: dynamic_pack_index
// CHECK: } // end sil function
public func test() -> (Int, String) {
  return variadicFunc(42, "hello")
}
