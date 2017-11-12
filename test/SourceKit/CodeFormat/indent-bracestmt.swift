func foo() {
    var xyz : Int
    if xyz == 2 {
    xyz = 1
    }

  if 2 == 2
    && 3 == 3 {
   let a = 2
	}
}

class Foo {
    static var thing: Int = {
        return 0
        }()
}

func Foo1() {
    let msg = String([65, 108, 105, 103, 110].map { c in
        Character(UnicodeScalar(c))
})
}

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=16 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=22 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    if xyz == 2 {"
// CHECK: key.sourcetext: "        xyz = 1"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "    let a = 2"
// CHECK: key.sourcetext: "    }()"
// CHECK: key.sourcetext: "    })"
