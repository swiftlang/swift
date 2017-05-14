func foo() {
  let s1 = """
this is line1,
     this is line2,
"""
  let s1 =
  "content"
}

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "this is line1,"
// CHECK: key.sourcetext: "     this is line2,"
// CHECK: key.sourcetext: "\"\"\""
// CHECK: key.sourcetext: "    \"content\""
