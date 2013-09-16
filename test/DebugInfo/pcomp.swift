// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

protocol A {
	func x()
}

protocol B {
	func y()
}

// CHECK: _TtP5pcomp1AS_1B_
func f (arg : protocol<A,B>) {
}

