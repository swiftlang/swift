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

func foo3() {
    var test : Int = 1
    switch (test)
    {
        // case 0
    case 0:
        println("It's zero")
        // case 1
    case 1:
        println("It's one")
        println("Really, it's one")
        // default
    default:
        println("It's something else")
    }
}

func foo4() {
    var test : Int = 1
    switch test {
        case 0:
            print()
        case 1:
            break
        default:
            break
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

// RUN: %sourcekitd-test -req=format -line=30 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=31 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=32 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=33 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=34 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=35 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=36 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=37 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=38 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=39 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=40 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=41 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=42 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -req-opts=indent_switch_case=1 -line=47 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -req-opts=indent_switch_case=1 -line=48 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -req-opts=indent_switch_case=1 -line=49 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -req-opts=indent_switch_case=1 -line=50 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -req-opts=indent_switch_case=1 -line=51 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -req-opts=indent_switch_case=1 -line=52 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -req-opts=indent_switch_case=1 -line=53 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -req-opts=indent_switch_case=1 -line=54 -length=1 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    switch (test) {"
// CHECK: key.sourcetext: "    case 0:"
// CHECK: key.sourcetext: "        println(\"It's zero\")"
// CHECK: key.sourcetext: "    case 1:"
// CHECK: key.sourcetext: "        println(\"It's one\")"
// CHECK: key.sourcetext: "        println(\"Really, it's one\")"
// CHECK: key.sourcetext: "    default:"
// CHECK: key.sourcetext: "        println(\"It's something else\")"
// CHECK: key.sourcetext: "    }"

// CHECK: key.sourcetext: "    switch (test)"
// CHECK: key.sourcetext: "    {"
// CHECK: key.sourcetext: "    case 0:"
// CHECK: key.sourcetext: "        println(\"It's zero\")"
// CHECK: key.sourcetext: "    case 1:"
// CHECK: key.sourcetext: "        println(\"It's one\")"
// CHECK: key.sourcetext: "        println(\"Really, it's one\")"
// CHECK: key.sourcetext: "    default:"
// CHECK: key.sourcetext: "        println(\"It's something else\")"
// CHECK: key.sourcetext: "    }"

// CHECK: key.sourcetext: "    switch (test)"
// CHECK: key.sourcetext: "    {"
// CHECK: key.sourcetext: "    // case 0"
// CHECK: key.sourcetext: "    case 0:"
// CHECK: key.sourcetext: "        println(\"It's zero\")"
// CHECK: key.sourcetext: "    // case 1"
// CHECK: key.sourcetext: "    case 1:"
// CHECK: key.sourcetext: "        println(\"It's one\")"
// CHECK: key.sourcetext: "        println(\"Really, it's one\")"
// CHECK: key.sourcetext: "    // default"
// CHECK: key.sourcetext: "    default:"
// CHECK: key.sourcetext: "        println(\"It's something else\")"
// CHECK: key.sourcetext: "    }"

// CHECK: key.sourcetext: "    switch test {"
// CHECK: key.sourcetext: "        case 0:"
// CHECK: key.sourcetext: "            print()"
// CHECK: key.sourcetext: "        case 1:"
// CHECK: key.sourcetext: "            break"
// CHECK: key.sourcetext: "        default:"
// CHECK: key.sourcetext: "            break"
// CHECK: key.sourcetext: "    }"
