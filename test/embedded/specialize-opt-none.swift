// RUN: %target-swift-emit-sil %s -parse-stdlib -O | %FileCheck %s
// RUN: %target-swift-emit-sil %s -parse-stdlib -O -enable-experimental-feature Embedded | %FileCheck %s --check-prefix EMBEDDED

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

struct X {}

struct MyStruct<T> {
	@_optimize(none)
	public func foo() {}
}

public func test() {
	MyStruct<X>().foo()
}

// CHECK: MyStruct.foo()
// CHECK: sil hidden [Onone] @$s4main8MyStructV3fooyyF : $@convention(method) <T> (MyStruct<T>) -> () {

// EMBEDDED: // specialized MyStruct.foo()
// EMBEDDED: sil [Onone] @$e4main8MyStructV3fooyyFAA1XV_Tg5 : $@convention(method) (MyStruct<X>) -> () {
