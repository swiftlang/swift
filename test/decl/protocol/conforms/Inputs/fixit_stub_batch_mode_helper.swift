protocol P {
	func foo() // expected-note{{protocol requires function 'foo()' with type '() -> ()'}}
}

class C {}