func foo(x: Bool) {
    guard x else {


// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: "        "
