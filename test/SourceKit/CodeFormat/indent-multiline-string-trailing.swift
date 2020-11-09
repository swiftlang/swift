// RUN: %sourcekitd-test -req=format -line=18 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=19 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=20 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=21 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=22 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=23 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    this is line1"
// CHECK: key.sourcetext: ""
// CHECK: key.sourcetext: "this is line2"
// CHECK: key.sourcetext: "  "
// CHECK: key.sourcetext: "  "

let s1 = """

this is line1

  this is line2


