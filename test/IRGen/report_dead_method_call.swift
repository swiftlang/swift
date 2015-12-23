// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift %S/Inputs/report_dead_method_call/main.swift %s -O -Xfrontend -disable-access-control -o %t/a.out
// RUN: %target-run %t/a.out 2> %t/err1.log; FileCheck %s < %t/err1.log
// RUN: %target-run %t/a.out p1 2> %t/err2.log; FileCheck %s < %t/err2.log
// RUN: %target-run %t/a.out p1 p2 2> %t/err3.log; FileCheck %s < %t/err3.log
// REQUIRES: executable_test


// The -disable-access-control option let us "call" methods, which are removed
// by dead method elimination.

// CHECK: fatal error: call of removed method

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

