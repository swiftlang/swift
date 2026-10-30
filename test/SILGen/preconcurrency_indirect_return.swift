// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen %s | %FileCheck %s

@preconcurrency
func test() -> (any Sendable)? { nil }

// CHECK-LABEL: sil {{.*}} @$s{{.*}}callWithPreconcurrency
func callWithPreconcurrency() {
  // CHECK-NOT: unchecked_addr_cast
  // CHECK: [[RESULT:%.*]] = alloc_stack $Optional<any Sendable>
  // CHECK: apply {{.*}}([[RESULT]])
  // CHECK: switch_enum_addr [[RESULT]]
  // CHECK: open_existential_addr
  // CHECK: init_existential_addr
  let x = test()
}
