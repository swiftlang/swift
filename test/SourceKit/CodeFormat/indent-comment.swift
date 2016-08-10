/**
* Foo Comment
*/
class Foo {
/**
* Bar Comment
*/
	func Bar() {}
}
/*
*
class Foo {}
*/
// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=11 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=12 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=38 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=39 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "/**"
// CHECK: key.sourcetext: " * Foo Comment"
// CHECK: key.sourcetext: " */"
// CHECK: key.sourcetext: "    /**"
// CHECK: key.sourcetext: "     * Bar Comment"
// CHECK: key.sourcetext: "     */"
// CHECK: key.sourcetext: " *"
// CHECK: key.sourcetext: " class Foo {}"
// CHECK: key.sourcetext: " *"
// CHECK: key.sourcetext: " func foo2() {}"

/*
*
func foo2() {}
