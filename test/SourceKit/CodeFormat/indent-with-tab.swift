class Foo {

var test : Int

	func foo() {
		test = 1
	}

}

func bar(a: Int,
b: Int) {}

// RUN: %sourcekitd-test -req=format -line=1 -length=1 -req-opts=usetabs=1 %s >%t.response
// RUN: %sourcekitd-test -req=format -line=2 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=3 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=4 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=5 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=6 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=7 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=8 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=9 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=12 -length=1 -req-opts=usetabs=1 %s >>%t.response
// RUN: %sourcekitd-test -req=format -line=12 -length=1 -req-opts=usetabs=1 -req-opts=tabwidth=0 %s >> %t.response
// RUN: %FileCheck --strict-whitespace %s <%t.response

// CHECK: key.sourcetext: "class Foo {"
// CHECK: key.sourcetext: "\t"
// CHECK: key.sourcetext: "\tvar test : Int"
// CHECK: key.sourcetext: "\t"
// CHECK: key.sourcetext: "\tfunc foo() {"
// CHECK: key.sourcetext: "\t\ttest = 1"
// CHECK: key.sourcetext: "\t}"
// CHECK: key.sourcetext: "\t"
// CHECK: key.sourcetext: "}"
// CHECK: key.sourcetext: "\t\t b: Int) {}"
// CHECK: key.sourcetext: "\t\t b: Int) {}"
