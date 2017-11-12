class Foo {
	func Bar() {
		repeat {
foo()


// RUN: %sourcekitd-test -req=format -line=1 -length=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "class Foo {"
// CHECK: key.sourcetext: "    func Bar() {"
// CHECK: key.sourcetext: "        repeat {"
// CHECK: key.sourcetext: "            foo()"

