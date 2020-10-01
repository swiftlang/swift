func foo(x: Bool) {
    if x {}
    else {

// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: "        "
