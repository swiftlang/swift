func foo(foo: Int, bar: Int, baz: Int, buzz: Int) -> Int {
    return foo + bar + baz + buzz
}

foo(0,
  bar: 1,
      baz: 2,
          buzz: 3)

// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: FileCheck --strict-whitespace %s <%t.response

//                        "foo(0,"
// CHECK: key.sourcetext: "    bar: 1,"

//                        "  bar: 1,"
// CHECK: key.sourcetext: "  baz: 2,"

//                        "      baz: 2,"
// CHECK: key.sourcetext: "      buzz: 3)"
