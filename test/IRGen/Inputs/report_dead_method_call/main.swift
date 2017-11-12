// The StdlibUnittest test suite is placed here because it contains
// expressions that are only allowed at the top level in files named
// "main.swift".

import StdlibUnittest

@inline(never)
func testProto(_ c: Container) {
	// call the dead witness method abc()
	c.p.abc()
}

@inline(never)
func testClass(_ c: ClassContainer) {
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
func testPublicClass(_ c: PublicBase) {
	// call the dead private vtable method ghi()
	c.ghi()
}

let ReportDeadMethodCallTestSuite = TestSuite("ReportDeadMethodCall")

ReportDeadMethodCallTestSuite.test("Call class") {
  expectCrashLater()
  callClass()
}

ReportDeadMethodCallTestSuite.test("Call proto") {
  expectCrashLater()
  callProto()
}

ReportDeadMethodCallTestSuite.test("Call public class") {
  expectCrashLater()
  callPublicClass()
}

runAllTests()

