class Foo {
    lazy var test: () -> Int = {
        return 1
    }
}

class Foo2 {
  lazy var b: {
    return
  }
}

// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "class Foo {"
// CHECK: key.sourcetext: "    lazy var test: () -> Int = {"
// CHECK: key.sourcetext: "        return 1"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "}"
