// RUN: %target-swift-emit-silgen -enable-experimental-feature AddressableParameters %s 

// REQUIRES: swift_feature_AddressableParameters

struct Foo {}

func foo(_: borrowing @_addressable Foo) {}

func bar() -> (Foo, Int)? { return nil }

func test1() {
	guard let (f, i) = bar() else {return}
	
	foo(f)
}

func test2() {
	if let (f, i) = bar() {foo(f)} else {}
}
