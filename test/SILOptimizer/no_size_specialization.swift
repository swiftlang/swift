// RUN: %target-swift-frontend  %s -O -emit-sil | %FileCheck %s -check-prefix=CHECK-O
// RUN: %target-swift-frontend  %s -Osize -emit-sil | %FileCheck %s -check-prefix=CHECK-OSIZE

@_semantics("optimize.sil.specialize.generic.size.never")
func foo<T>(_ t: T) -> T {
  return t
}

// CHECK-O-LABEL: sil @{{.*}}test
// CHECK-O: %0 = integer_literal
// CHECK-O: %1 = struct $Int
// CHECK-O: return %1

// CHECK-OSIZE-LABEL: sil {{.*}} @{{.*}}foo

// CHECK-OSIZE-LABEL: sil @{{.*}}test
// CHECK-OSIZE: function_ref {{.*}}foo
// CHECK-OSIZE: apply
public func test() -> Int {
  return foo(27)
}
