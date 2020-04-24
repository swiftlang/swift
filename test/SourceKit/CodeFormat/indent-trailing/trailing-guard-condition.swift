func foo(x: Bool) {
    guard let number = patterns.index(where: {

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: "        "
