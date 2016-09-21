class Foo {
    var test: Int {

    }
}

class Foo {
    var test: Int {
        let result = 1
        return result
    }

    subscript(a: Int) -> Int {
        let x = 3
        return x
    }
}

class Foo1 {
var foo: Int {
    return 1
  }
class var foo: Int {
  return 1
  }
}
// REQUIRES: disabled
// FIXME: may be crashing non-deterministically

// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=11 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=12 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=13 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=14 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=15 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=16 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=17 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=20 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=23 -length=1 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "class Foo {"
// CHECK: key.sourcetext: "    var test: Int {"
// CHECK: key.sourcetext: "        "
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "}"

// CHECK: key.sourcetext: "class Foo {"
// CHECK: key.sourcetext: "    var test: Int {"
// CHECK: key.sourcetext: "        let result = 1"
// CHECK: key.sourcetext: "        return result"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    subscript(a: Int) -> Int {"
// CHECK: key.sourcetext: "        let x = 3"
// CHECK: key.sourcetext: "        return x"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "}"
// CHECK: key.sourcetext: "    var foo: Int {"
// CHECK: key.sourcetext: "    class var foo: Int {"
