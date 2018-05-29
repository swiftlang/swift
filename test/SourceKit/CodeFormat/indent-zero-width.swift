class Foo {

var test : Int

	func foo() {
		test = 1
	}

}
func bar(a: Int,
b: Int) {}

// RUN: %sourcekitd-test -req=format -line=1 -length=1 -req-opts=usetabs=0 -req-opts=indentwidth=0 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 -req-opts=usetabs=0 -req-opts=indentwidth=0 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 -req-opts=usetabs=0 -req-opts=indentwidth=0 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=11 -length=1 -req-opts=usetabs=0 -req-opts=indentwidth=0 %s >>%t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "class Foo {"
// CHECK: key.sourcetext: "var test : Int"
// CHECK: key.sourcetext: "    test = 1"
// CHECK: key.sourcetext: "         b: Int) {}"
