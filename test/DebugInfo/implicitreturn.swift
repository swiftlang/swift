// RUN: %swift -target x86_64-apple-darwin10 %s -S -g -o - | FileCheck %s
func app() {
// Make sure we don't jump back to before the prologue.
// CHECK: .loc	1 [[@LINE+2]]
// CHECK-NOT: .loc	1 [[@LINE-3]] 
        var x : Bool = true
        x = !x
        x = !x
// CHECK:	.loc	1 [[@LINE+3]] 1
// CHECK-NOT:.loc
// CHECK:	ret
}

app()
