// RUN: %empty-directory(%t)

// We compile with -O (optimizations) and -disable-access-control (which
// allows use to "call" methods that are removed by dead code elimination).
// RUN: %target-build-swift %S/Inputs/report_dead_method_call/main.swift %s -O -Xfrontend -disable-access-control -o %t/report_dead_method_call

// The private, unused methods are optimized away. The test calls these
// methods anyway (since it has overridden the access control), so we
// expect them to produce "Fatal error: Call of deleted method" when run.
// RUN: %target-codesign %t/report_dead_method_call
// RUN: %target-run %t/report_dead_method_call
// REQUIRES: executable_test

private protocol PrivateProto {
	func abc()
}

struct PrivateStructC : PrivateProto {
	func abc() {
	}
}

struct Container {

	private var p: PrivateProto = PrivateStructC()
}

@inline(never)
func callProto() {
	testProto(Container())
}

private class Base {
	func def() {
	}
}

private class Derived : Base {
	override func def() {
	}
}

struct ClassContainer {

	private var p: Base = Derived()
}

@inline(never)
func callClass() {
	testClass(ClassContainer())
}

public class PublicBase {
	private func ghi() {
	}
}

