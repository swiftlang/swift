// RUN: %target-swift-emit-silgen %s | %FileCheck %s

@preconcurrency
func test() -> (any Sendable)? { nil }

// CHECK-LABEL: sil {{.*}} @$s{{.*}}callWithPreconcurrency
func callWithPreconcurrency() {
	// CHECK: unchecked_addr_cast {{.*}} to $*Optional<any Sendable>
	let x = test()
}
