// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

do {
    defer { print("deferred 1") }
    defer { print("deferred 2") }
    print("start!")

    // CHECK-NOT: deferred
    // CHECK-LABEL: start!
    // CHECK-NEXT: deferred 2
    // CHECK-NEXT: deferred 1
}

// ensure #function ignores defer blocks
do {
    print("top-level #function")
    let name = #function
    defer { print(name == #function ? "good" : "bad") }

    // CHECK-LABEL: top-level #function
    // CHECK-NEXT: good
}

func foo() {
    print("foo()")
    let name = #function
    defer { print(name == #function ? "good" : "bad") }

    // CHECK-LABEL: foo()
    // CHECK-NEXT: good
}
foo()
