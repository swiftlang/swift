func foo(x: Bool) {
    guard let number = Optional(23),

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: "          "
