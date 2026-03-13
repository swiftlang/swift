// RUN: %target-swift-frontend  -primary-file %s -O -emit-sil | %FileCheck %s

// Check that no Array code is de-serialized due to specialization attributes

// CHECK-NOT: Array

@inline(never)
func foo<T>(_ x: T) {
}

public func test() {
  foo(27)
}
