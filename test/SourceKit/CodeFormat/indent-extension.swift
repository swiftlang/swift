class Foo {
}

extension Foo {

}

extension Foo
{

}


// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=11 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "extension Foo {"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "}"

// CHECK: key.sourcetext: "extension Foo"
// CHECK: key.sourcetext: "{"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "}"
