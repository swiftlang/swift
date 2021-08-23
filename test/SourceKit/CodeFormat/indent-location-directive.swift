#sourceLocation(file: "someFile.swift", line: 10)

    func foo() {
// RUN: %sourcekitd-test -req=format -pos=%(line+1):1 %s | %FileCheck %s
let test = 1
// CHECK: key.sourcetext: "        let test = 1"
    }
#sourceLocation()
