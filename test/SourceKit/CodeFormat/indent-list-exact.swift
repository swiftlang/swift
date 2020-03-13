class Foo {
  func foo(Value1 : Int,
    Value2 : Int) {
  }
func bar(value1 : Int,
value2 : Int,
        value3 : Int) {
  }
    func foo1(Value1 : Int,
    Value2 : Int) {}
    func foo3() {
      foo(1,
  2)
    foo(2,
3)
        foo(3,
4)
    }
    func intGen() -> Int { return 0 }
    func foo4() {
      var a = [1,
      2,
            3,
  1 + 2,
          intGen()]
    }
    func foo5() {
      var a = [1 : 1,
            2 : 2,
          3 + 2 : 3,
        intGen() : intGen()]
        var b = (2,
                   3,
                     4,
                       5)
    }
    func foo6<T1: Testable,
     T2: Testable,
           T3: Testable,
T4: where T4 : Testable>(t1 : T1, t2 : T2, t3 : T2) {}
    func foo7(i1: Int, i2: Int,
                       i3: Int, i4: Int,
                                i5 : Int) {}
    func foo7(i1 : Int,
              i2 : Int) {
var a : Int
    }
}
protocol Testable {}

class Foo1 {
  func foo1(i : Int,
  ) {}
  func foo2(i : Int, j : Int,
  )
}

class Foo2 {
  func foo1(i : Int, j : Int, k : Int) {}
  func foo2(i : Int, j : Int) {}
  func foo3() {
    foo1(1,
    )
    foo2(1,
    j : 1)
    foo2(1, j: 2,
    )
    foo2(1,

  }
}

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=13 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=15 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=17 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=22 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=23 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=24 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=25 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=29 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=30 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=31 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=33 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=34 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=35 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=38 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=39 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=40 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=42 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=43 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=46 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=53 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=55 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=63 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=65 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=67 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=69 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

//                        "  func foo(Value1 : Int,"
// CHECK: key.sourcetext: "           Value2 : Int) {"

//                        "func bar(value1 : Int,"
// CHECK: key.sourcetext: "         value2 : Int,"

//                        "value2 : Int,"
// CHECK: key.sourcetext: "value3 : Int) {"

//                        "    func foo1(Value1 : Int,"
// CHECK: key.sourcetext: "              Value2 : Int) {}"

//                        "      foo(1,"
// CHECK: key.sourcetext: "          2)"

//                        "    foo(2,"
// CHECK: key.sourcetext: "        3)"

//                        "        foo(3,"
// CHECK: key.sourcetext: "            4)"

//                        "      var a = [1,"
// CHECK: key.sourcetext: "               2,"

//                        "      2,"
// CHECK: key.sourcetext: "      3,"

//                        "            3,"
// CHECK: key.sourcetext: "            1 + 2,"

//                        "  1 + 2,"
// CHECK: key.sourcetext: "  intGen()]"

//                        "      var a = [1 : 1,"
// CHECK: key.sourcetext: "               2 : 2,"

//                        "            2 : 2,"
// CHECK: key.sourcetext: "            3 + 2 : 3,"

//                        "          3 + 2 : 3,"
// CHECK: key.sourcetext: "          intGen() : intGen()]"

//                        "        var b = (2,"
// CHECK: key.sourcetext: "                 3,"

//                        "                   3,"
// CHECK: key.sourcetext: "                   4,"

//                        "                     4,"
// CHECK: key.sourcetext: "                     5)"

//                        "    func foo6<T1: Testable,"
// CHECK: key.sourcetext: "              T2: Testable,"

//                        "     T2: Testable,"
// CHECK: key.sourcetext: "     T3: Testable,"

//                        "           T3: Testable,"
// CHECK: key.sourcetext: "           T4: where T4 : Testable>(t1 : T1, t2 : T2, t3 : T2) {}"

//                        "    func foo7(i1: Int, i2: Int,"
// CHECK: key.sourcetext: "              i3: Int, i4: Int,"

//                        "                       i3: Int, i4: Int,"
// CHECK: key.sourcetext: "                       i5 : Int) {}"

//                        "    func foo7(i1 : Int,"
// CHECK: key.sourcetext: "        var a : Int"

//                        "  func foo1(i : Int,"
// CHECK: key.sourcetext: "            ) {}"

//                        "  func foo2(i : Int, j : Int,"
// CHECK: key.sourcetext: "            )"

//                        "    foo1(1,"
// CHECK: key.sourcetext: "         )"

//                        "    foo2(1,"
// CHECK: key.sourcetext: "         j : 1)"

//                        "    foo2(1, j: 2,"
// CHECK: key.sourcetext: "         )"

//                        "    foo2(1,"
// CHECK: key.sourcetext: "         "
