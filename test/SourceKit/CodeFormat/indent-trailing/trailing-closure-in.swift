func foo() {
    foo { (
        x, 
        y
    ) in

// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: "        "
