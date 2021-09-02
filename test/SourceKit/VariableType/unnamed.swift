let = 2
let : Int = 4

let x: (String) -> Void = { (: String) in }

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s | %FileCheck %s
// CHECK: <VariableTypes>
// CHECK-NEXT: (4:5, 4:6): (String) -> Void (explicit type: 1)
// CHECK-NEXT: </VariableTypes>

