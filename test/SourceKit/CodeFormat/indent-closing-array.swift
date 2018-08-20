struct Foo {
    let bar = [
        1,
        2,
    ]

    func baz() {
        let qux = [
            "a": 1,
            "b": 2,
        ]

        let quxx = [
            "a": 1,
            "b": 2
        ]
    }
}

// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=11 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=16 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response
// CHECK: key.sourcetext: "    ]"
// CHECK: key.sourcetext: "        ]"
// CHECK: key.sourcetext: "        ]"
