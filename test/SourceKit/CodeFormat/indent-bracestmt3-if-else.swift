func foo() {
    var abc = 1
    if abc == 1 {
        abc = 2
    }
    else if abc == 2 {
        abc = 3
    } else {
        abc = 3
    }
}

struct Foo {
internal var foo: Int { return 0 }
static var bar: Int { return 1 }
private func baz() -> Int { return 2 }
}

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=14 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=15 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=16 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    if abc == 1 {"
// CHECK: key.sourcetext: "        abc = 2"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "    else if abc == 2 {"
// CHECK: key.sourcetext: "        abc = 3"
// CHECK: key.sourcetext: "    } else {"
// CHECK: key.sourcetext: "        abc = 3"
// CHECK: key.sourcetext: "    }"

// CHECK: key.sourcetext: "    internal var foo: Int { return 0 }"
// CHECK: key.sourcetext: "    static var bar: Int { return 1 }"
// CHECK: key.sourcetext: "    private func baz() -> Int { return 2 }"
