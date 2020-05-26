@IBAction func foo() {
    var xyz : Int
}
@IBSegueAction func bar() {
    var xyz : Int
}

// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >%t.action.response
// RUN: %FileCheck --strict-whitespace %s <%t.action.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >%t.segueaction.response
// RUN: %FileCheck --strict-whitespace %s <%t.segueaction.response

// CHECK: key.sourcetext: "    var xyz : Int"
