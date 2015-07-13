protocol Fooable {
	func foo()
}

extension FooStruct : Fooable {
	func foo() { }
}
