// RUN: %sourcekitd-test -req=format -line=24 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=25 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=26 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=27 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=28 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=29 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=30 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=31 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// The end quotes are ignored as it would be impossible to know whether
// they are part of the interpolation or another string, etc.
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    this is line1"
// CHECK: key.sourcetext: ""
// CHECK: key.sourcetext: "this is line2"
// CHECK: key.sourcetext: "    "
// CHECK: key.sourcetext: "    and this is a line with trailing interpolation \\(1 +"
// CHECK: key.sourcetext: " "
// CHECK: key.sourcetext: " \"\"\""

let s1 = """

this is line1

    this is line2

 and this is a line with trailing interpolation \(1 +

  """
