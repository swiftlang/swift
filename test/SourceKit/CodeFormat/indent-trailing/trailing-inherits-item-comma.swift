struct Foo: Equatable,

// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s | %FileCheck --strict-whitespace %s
// CHECK: key.sourcetext: "            "
