func foo() {
    var test : Int = 1
    switch (test) {
    case 0:
        println("It's zero")
    case 1:
        println("It's one")
        println("Really, it's one")
    default:
        println("It's something else")
    }
}

func foo2() {
    var test : Int = 1
    switch (test)
    {
    case 0:
        println("It's zero")
    case 1:
        println("It's one")
        println("Really, it's one")
    default:
        println("It's something else")
    }
}

// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=11 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=16 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=17 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=18 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=19 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=20 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=21 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=22 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=23 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=24 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=25 -length=1 %s >>%t.response

// RUN: FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    switch (test) {"
// CHECK: key.sourcetext: "    case 0:"
// CHECK: key.sourcetext: "        println("It's zero")"
// CHECK: key.sourcetext: "    case 1:"
// CHECK: key.sourcetext: "        println("It's one")"
// CHECK: key.sourcetext: "        println("Really, it's one")"
// CHECK: key.sourcetext: "    default:"
// CHECK: key.sourcetext: "        println("It's something else")"
// CHECK: key.sourcetext: "    }"

// CHECK: key.sourcetext: "    switch (test)"
// CHECK: key.sourcetext: "    {"
// CHECK: key.sourcetext: "    case 0:"
// CHECK: key.sourcetext: "        println("It's zero")"
// CHECK: key.sourcetext: "    case 1:"
// CHECK: key.sourcetext: "        println("It's one")"
// CHECK: key.sourcetext: "        println("Really, it's one")"
// CHECK: key.sourcetext: "    default:"
// CHECK: key.sourcetext: "        println("It's something else")"
// CHECK: key.sourcetext: "    }"
