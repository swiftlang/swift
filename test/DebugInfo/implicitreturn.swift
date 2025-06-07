// RUN: %target-swift-frontend %s -S -g -o - | %FileCheck %s

func app() {
// Make sure we don't jump back to before the prologue.
// CHECK: .loc	[[FILEID:[0-9]]] [[@LINE+2]]
// CHECK-NOT: .loc	[[FILEID]] [[@LINE-3]]
        var x : Bool = true
        x = !x
        x = !x
// CHECK:	.loc	[[FILEID]] [[@LINE+2]] 1
// CHECK:	ret
}

app()

