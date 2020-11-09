func foo() {
  let s1 = """

this is line1

     this is line2


  this is line3
 this is a line with interpolation \(1 +
    2)
    """

  let s2 = """
"""
}

// RUN: %sourcekitd-test -req=format -line=2 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=3 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=10 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=11 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=12 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=15 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    let s1 = \"\"\""
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    this is line1"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "     this is line2"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    this is line3"
// CHECK: key.sourcetext: "    this is a line with interpolation \\(1 +"
// CHECK: key.sourcetext: "                                        2)"
// CHECK: key.sourcetext: "    \"\"\""
// CHECK: key.sourcetext: "\"\"\""
