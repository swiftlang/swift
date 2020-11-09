let s1 = """
  \(
    45
)
  """

let s2 = """
  \(
    45
  )
"""

let s3 = """
\(
  45
)
  """

let s4 = """
  foo \( 45 /*
comment content
*/) bar
  """

// RUN: %sourcekitd-test -req=format -line=2 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=3 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=8 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=10 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=14 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=15 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=16 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=20 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=21 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=22 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "  \\("
// CHECK: key.sourcetext: "    45"
// CHECK: key.sourcetext: "  )"

// CHECK: key.sourcetext: "  \\("
// CHECK: key.sourcetext: "    45"
// CHECK: key.sourcetext: "  )"

// CHECK: key.sourcetext: "  \\("
// CHECK: key.sourcetext: "    45"
// CHECK: key.sourcetext: ")"

// CHECK: key.sourcetext: "  foo \\( 45 /*"
// CHECK: key.sourcetext: "         comment content"
// CHECK: key.sourcetext: "         */) bar"
