// RUN: %target-swift-emit-silgen -enable-experimental-feature AddressableParameters %s | %FileCheck %s
// REQUIRES: swift_feature_AddressableParameters
struct Foo {
	var x, y, z: Int

	init() { fatalError() }

	@_addressableSelf
	func foo() {}
}

func block(_: (Int) -> ()) {}

func test() {
	let x = Foo()
	block { (_) in x.foo() }
}
// ensure the closure properly nests the store_borrow after the alloc_stack:
// CHECK-LABEL: sil{{.*}} @${{.*}}4test{{.*}}U_ : $
// CHECK: [[STACK:%.*]] = alloc_stack
// CHECK-NEXT: store_borrow {{.*}} to [[STACK]]
