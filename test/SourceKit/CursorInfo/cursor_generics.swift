func testGenerics<T>(x: T) {
}

/// Doc Comment...
func testGenericsWithComment<T>(x: T) {
}

func someFunc <A>() -> A {
    fatalError()
}

// rdar://problem/36871908
class MyType<T> {
	let test: Bool = false
	let items: [Int] = []
	func myMethod() {
	  if test {}
	  for i in items {}
	}
}

// RUN: %sourcekitd-test -req=cursor -pos=1:10 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: <Declaration>func testGenerics&lt;T&gt;(x: <Type usr="s:15cursor_generics12testGenerics1xyx_tlF1TL_xmfp">T</Type>)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=5:10 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: <Function
// CHECK2: <Declaration>func testGenericsWithComment&lt;T&gt;(x: T)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=8:16 %s -- %s | %FileCheck -check-prefix=CHECK3 %s
// CHECK3: source.lang.swift.decl.generic_type_param
// CHECK3: <Declaration>A</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=17:8 %s -- %s | %FileCheck -check-prefix=CHECK4 %s
// CHECK4: source.lang.swift.ref.var.instance
// CHECK4: <Declaration>let test: <Type usr="s:Sb">Bool</Type></Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=18:14 %s -- %s | %FileCheck -check-prefix=CHECK5 %s
// CHECK5: source.lang.swift.ref.var.instance
// CHECK5: <Declaration>let items: [<Type usr="s:Si">Int</Type>]</Declaration>
