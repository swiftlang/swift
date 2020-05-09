class Foo
{
    func foo()
    {
	bar()
        {
        }
    }
}

// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "class Foo"
// CHECK: key.sourcetext: "{"
// CHECK: key.sourcetext: "    func foo()"
// CHECK: key.sourcetext: "    {"
// CHECK: key.sourcetext: "        bar()"

//                             bar()"
// CHECK: key.sourcetext: "    {"
// CHECK: key.sourcetext: "    }"

//                             func foo()"
//                             {"
//                                 ..."
// CHECK: key.sourcetext: "    }"

//                         class Foo"
//                             ..."
// CHECK: key.sourcetext: "}"
