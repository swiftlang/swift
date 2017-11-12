@IBAction func foo() {
    var xyz : Int
}

// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    var xyz : Int"
