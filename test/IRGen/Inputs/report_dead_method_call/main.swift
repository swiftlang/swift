

@inline(never)
func testProto(c: Container) {
	// call the dead witness method abc()
	c.p.abc()
}

@inline(never)
func testClass(c: ClassContainer) {
	// call the dead vtable method def()
	c.p.def()
}

public class PublicDerived : PublicBase {
	// The vtable of PublicDerived contains a reference to PublicBase.ghi()
}

@inline(never)
func callPublicClass() {
	testPublicClass(PublicDerived())
}

@inline(never)
func testPublicClass(c: PublicBase) {
	// call the dead private vtable method ghi()
	c.ghi()
}

switch Process.argc {
case 1:
	callClass()

case 2:
	callProto()

case 3:
	callPublicClass()

default:
	break
}
