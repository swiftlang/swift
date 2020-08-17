func foo() {
    let s1 = """
        this is line1 in outer string \("""
  nested string in interpolation
            """)

    this is line2 in outer string
        """
}

// RUN: %sourcekitd-test -req=format -line=3 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=4 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "        this is line1 in outer string \\(\"\"\""
// CHECK: key.sourcetext: "            nested string in interpolation"
// CHECK: key.sourcetext: "            \"\"\")"
// CHECK: key.sourcetext: "        "
// CHECK: key.sourcetext: "        this is line2 in outer string"
// CHECK: key.sourcetext: "        \"\"\""
