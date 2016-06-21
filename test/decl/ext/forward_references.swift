// RUN: %target-swift-frontend -primary-file %s %S/../../Inputs/forward_extension_reference.swift -emit-ir -g -module-name fref
// RUN: %target-swift-frontend %S/../../Inputs/forward_extension_reference.swift -primary-file %s -emit-ir -g -module-name fref

struct Foo<T> {
	func foo(_ t: T) -> T {
		return t
	}

	var _countAndFlags: UInt = 0

	func gar() -> Int {
		return count
	}
}

func goo<T>(_ f: Foo<T>) {
	var x = f.count
}

protocol Bar {
	var count: Int {get set}
}
