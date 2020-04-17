func first() {
    foo
        .bar {
        }
        .tug

    baz
        .bop
.
}

qux
    .wop
.
;

func second() {
    baz
        .bop()
    .
}

func third() {
    baz {

    }
.
}

func fourth() {
    baz() {

    }
.
}

func fifth() {
    baz
        .bar {}
.
}

func sixth() {
    baz
        .bar(
        ) {

        }
.
}


// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=14 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=20 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=27 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=34 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=40 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=49 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response
//
//                        "    foo"
//                        "        .bar {"
// CHECK: key.sourcetext: "        }"
//
//                        "    foo"
//                        "        .bar {"
//                        "        }
// CHECK: key.sourcetext: "        .tug"
//
//                        "    baz"
//                        "        .bop"
// CHECK: key.sourcetext: "        ."
//
//                        "qux"
//                        "    .wop"
// CHECK: key.sourcetext: "    ."
//
//                        "    baz"
//                        "        .bop()"
// CHECK: key.sourcetext: "        ."
//
//                        "    baz {"
//                        "    }"
// CHECK: key.sourcetext: "    ."
//
//                        "    baz"
//                        "        .bar {}"
// CHECK: key.sourcetext: "        ."
//
//                        "    baz
//                        "        .bar("
//                        "        ) {"
//                        "        }"
// CHECK: key.sourcetext: "        ."
