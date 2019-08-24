func foo1() {
	class Base {}
	class Derived : Base {}

	var b : Base
	b as Derived
	undeclared_foo1
}
