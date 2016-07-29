class Foo {

var test : Int

    func foo() {
test = 1
 }

}

func foo(a a: [Int: Int]) {}
foo(a: [
  3: 3
])

precedencegroup Whatever {
x
}

// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=13 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=17 -length=1 %s >>%t.response
// RUN: FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "class Foo {"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    var test : Int"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    func foo() {"
// CHECK: key.sourcetext: "        test = 1"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "}"
//                        "foo(a: ["
// CHECK: key.sourcetext: "    3: 3"
// CHECK: key.sourcetext: "    x"
