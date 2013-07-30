// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s
struct Foo {
// CHECK: [ DW_TAG_subprogram ] [line [[@LINE+1]]] [def] [constructor]
	constructor (x : Int) {}
	func bar (x : Int) {}
}

var f = Foo(1)
f.bar(2)

