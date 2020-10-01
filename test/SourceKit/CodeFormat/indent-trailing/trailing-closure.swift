func foo() {
    bar
        .fooooooooooo(first: 3,
                      second)[x: 10] {

// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: "            "
