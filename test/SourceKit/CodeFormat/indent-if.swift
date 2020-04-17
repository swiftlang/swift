if condition,
        !condition,
         condition,
    condition,
    !condition,
              condition {
}


// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "   !condition,"
// CHECK: key.sourcetext: "        condition,"
// CHECK: key.sourcetext: "         condition,"
// CHECK: key.sourcetext: "    !condition,"
// CHECK: key.sourcetext: "    condition {"
