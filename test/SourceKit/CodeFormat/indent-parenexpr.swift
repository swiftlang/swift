func foo() {
    bar(

    )

    bar({

    })

    bar(abc, {

    })
    
    bar((

    ))

    bar(
        (
        )
    )

}

// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=11 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=12 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=14 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=15 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=16 -length=1 %s >>%t.response

// RUN: %sourcekitd-test -req=format -line=18 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=19 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=20 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=21 -length=1 %s >>%t.response

// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    bar("
// CHECK: key.sourcetext: "        "
// CHECK: key.sourcetext: "    )"

// CHECK: key.sourcetext: "    bar({"
// CHECK: key.sourcetext: "        "
// CHECK: key.sourcetext: "    })"

// CHECK: key.sourcetext: "    bar(abc, {"
// CHECK: key.sourcetext: "        "
// CHECK: key.sourcetext: "    })"

// CHECK: key.sourcetext: "    bar(("
// CHECK: key.sourcetext: "        "
// CHECK: key.sourcetext: "    ))"

// CHECK: key.sourcetext: "    bar("
// CHECK: key.sourcetext: "        ("
// CHECK: key.sourcetext: "        )"
// CHECK: key.sourcetext: "    )"
