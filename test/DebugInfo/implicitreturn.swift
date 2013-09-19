// RUN: %swift -triple x86_64-apple-darwin10 %s -S -g -o - | FileCheck %s

func app() {
	var x : Bool = true
	x = !x
	x = !x
// CHECK:	.loc	1 [[@LINE+3]] 1
// CHECK-NOT:.loc
// CHECK:	ret
}

app()
