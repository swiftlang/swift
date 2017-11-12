func foo() {
    do {
    foo()
    }
    catch let e as MSV {
    foo()
    } catch let e as MSV2 {
    foo()
    } catch _
    {
    foo()
    }
    catch
    {
    foo()
    }
}

// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=10 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=11 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=12 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=13 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=14 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=15 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=16 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "    do {"
// CHECK: key.sourcetext: "        foo()"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "    catch let e as MSV {"
// CHECK: key.sourcetext: "        foo()"
// CHECK: key.sourcetext: "    } catch let e as MSV2 {"
// CHECK: key.sourcetext: "        foo()"
// CHECK: key.sourcetext: "    } catch _"
// CHECK: key.sourcetext: "    {"
// CHECK: key.sourcetext: "        foo()"
// CHECK: key.sourcetext: "    }"
// CHECK: key.sourcetext: "    catch"
// CHECK: key.sourcetext: "    {"
// CHECK: key.sourcetext: "        foo()"
// CHECK: key.sourcetext: "    }"
