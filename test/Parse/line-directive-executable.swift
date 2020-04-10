// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

print("START") // CHECK-LABEL: START

func check(file: String = #file, filePath: String = #filePath, line: Int = #line) {
    print("\(filePath):\(line):\(file)")
}

#sourceLocation(file: "a.swift", line: 100)
check() // CHECK-NEXT: {{^}}a.swift:100:main/a.swift

public struct S {
    #sourceLocation(file: "b.swift", line: 100)
    func foo() { check() }

    #sourceLocation(file: "c.swift", line: 200)

    func bar() { check() }

    #sourceLocation(file: "d.swift", line: 300)
}
check() // CHECK-NEXT: {{^}}d.swift:301:main/d.swift
S().foo() // CHECK-NEXT: {{^}}b.swift:100:main/b.swift
S().bar() // CHECK-NEXT: {{^}}c.swift:201:main/c.swift

enum E {
#sourceLocation(file: "e.swift", line: 400)
}
check() // CHECK-NEXT: {{^}}e.swift:401:main/e.swift

class C {
#sourceLocation()
}
check() // CHECK-NEXT: .swift:[[@LINE]]:main/line-directive-executable.swift

extension C {
#sourceLocation(file: "f.swift", line: 500)
    static var file: String { return #file }
    static var filePath: String { return #filePath }

    #sourceLocation(file: "g.swift", line: 600)
    var line: Int { return #line }

#sourceLocation(file: "h.swift", line: 700)
}
check() // CHECK-NEXT: {{^}}h.swift:701:main/h.swift
check(file: C.file, filePath: C.filePath, line: C().line) // CHECK-NEXT: {{^}}f.swift:600:main/f.swift

func test() {
#sourceLocation(file: "i.swift", line: 800)
    check()
#sourceLocation(file: "j.swift", line: 900)
}

check() // CHECK-NEXT: {{^}}j.swift:902:main/j.swift
test() // CHECK-NEXT: {{^}}i.swift:800:main/i.swift

#sourceLocation()
check() // CHECK-NEXT: .swift:[[@LINE]]:main/line-directive-executable.swift

#sourceLocation(file: "k.swift", line: 1000)


check() // CHECK-NEXT: {{^}}k.swift:1002:main/k.swift
