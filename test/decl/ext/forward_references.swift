// RUN: %swift -primary-file %s %S/../../Inputs/forward_extension_reference.swift -target x86_64-apple-darwin10 -emit-ir -g -module-name fref
// RUN: %swift %S/../../Inputs/forward_extension_reference.swift -primary-file %s -target x86_64-apple-darwin10 -emit-ir -g -module-name fref

struct Foo<T> {
	func foo(t: T) -> T {
		return t
	}

	var _countAndFlags: UInt = 0

	func gar() -> Int {
		return count
	}
}

func goo<T>(f: Foo<T>) {
	var x = f.count
}

protocol Bar {
	var count: Int {get set}
}
