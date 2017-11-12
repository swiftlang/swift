let a = 1
let b = 2 // End line with CRlet c = 3



// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "let a = 1"
// CHECK: key.sourcetext: "let b = 2 // End line with CR"
// CHECK: key.sourcetext: "let c = 3"

