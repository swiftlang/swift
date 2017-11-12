// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

print("START") // CHECK-LABEL: START

func check(file: String = #file, line: Int = #line) {
    print("\(file):\(line)")
}

#sourceLocation(file: "a.swift", line: 100)
check() // CHECK-NEXT: {{^}}a.swift:100

public struct S {
    #sourceLocation(file: "b.swift", line: 100)
    func foo() { check() }

    #sourceLocation(file: "c.swift", line: 200)

    func bar() { check() }

    #sourceLocation(file: "d.swift", line: 300)
}
check() // CHECK-NEXT: {{^}}d.swift:301
S().foo() // CHECK-NEXT: {{^}}b.swift:100
S().bar() // CHECK-NEXT: {{^}}c.swift:201

enum E {
#sourceLocation(file: "e.swift", line: 400)
}
check() // CHECK-NEXT: {{^}}e.swift:401

class C {
#sourceLocation()
}
check() // CHECK-NEXT: .swift:[[@LINE]]

extension C {
#sourceLocation(file: "f.swift", line: 500)
    static var file: String { return #file }

    #sourceLocation(file: "g.swift", line: 600)
    var line: Int { return #line }

#sourceLocation(file: "h.swift", line: 700)
}
check() // CHECK-NEXT: {{^}}h.swift:701
check(file: C.file, line: C().line) // CHECK-NEXT: {{^}}f.swift:600

func test() {
#sourceLocation(file: "i.swift", line: 800)
    check()
#sourceLocation(file: "j.swift", line: 900)
}

check() // CHECK-NEXT: {{^}}j.swift:902
test() // CHECK-NEXT: {{^}}i.swift:800

#sourceLocation()
check() // CHECK-NEXT: .swift:[[@LINE]]

#sourceLocation(file: "k.swift", line: 1000)


check() // CHECK-NEXT: {{^}}k.swift:1002
