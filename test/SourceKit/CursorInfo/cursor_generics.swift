func testGenerics<T>(x: T) {
}

/// Doc Comment...
func testGenericsWithComment<T>(x: T) {
}

func someFunc <A>() -> A {
    fatalError()
}

// RUN: %sourcekitd-test -req=cursor -pos=1:10 %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// CHECK1: <Declaration>func testGenerics&lt;T&gt;(x: <Type usr="s:15cursor_generics12testGenericsyx1x_tlF1TL_xmfp">T</Type>)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=5:10 %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// CHECK2: <Function
// CHECK2: <Declaration>func testGenericsWithComment&lt;T&gt;(x: T)</Declaration>

// RUN: %sourcekitd-test -req=cursor -pos=8:16 %s -- %s | %FileCheck -check-prefix=CHECK3 %s
// CHECK3: source.lang.swift.decl.generic_type_param
// CHECK3: <Declaration>A</Declaration>
