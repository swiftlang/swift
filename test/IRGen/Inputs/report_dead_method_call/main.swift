

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

if C_ARGC == 1 {
	callProto()
} else {
	callClass()
}
