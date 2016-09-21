for i in 0...5 {
}

var someProperty: String {

}
// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "for i in 0...5 {"
// CHECK: key.sourcetext: "}"
// CHECK: key.sourcetext: "    "
