@MainActor
protocol P {}

class InferMainActor: P {
  func test() {}
}

// RUN: %sourcekitd-test -req=cursor -pos=4:7 %s -- %s -module-name ConcurrencyTest | %FileCheck %s --check-prefix=CHECK-CLASS
// CHECK-CLASS: source.lang.swift.decl.class
// CHECK-CLASS-NEXT: InferMainActor
// CHECK-CLASS: <Declaration>@<Type usr="s:ScM">MainActor</Type> class InferMainActor : <Type usr="s:15ConcurrencyTest1PP">P</Type></Declaration>
// CHECK-CLASS: <decl.class><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@<ref.class usr="s:ScM">MainActor</ref.class></syntaxtype.attribute.name></syntaxtype.attribute.builtin> <syntaxtype.keyword>class</syntaxtype.keyword> <decl.name>InferMainActor</decl.name> : <ref.protocol usr="s:15ConcurrencyTest1PP">P</ref.protocol></decl.class>

// RUN: %sourcekitd-test -req=cursor -pos=5:8 %s -- %s -module-name ConcurrencyTest | %FileCheck %s --check-prefix=CHECK-FUNC
// CHECK-FUNC: source.lang.swift.decl.function.method.instance
// CHECK-FUNC-NEXT: test()
// CHECK-FUNC: <Declaration>@<Type usr="s:ScM">MainActor</Type> func test()</Declaration>
// CHECK-FUNC: <decl.function.method.instance><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@<ref.class usr="s:ScM">MainActor</ref.class></syntaxtype.attribute.name></syntaxtype.attribute.builtin> <syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>test</decl.name>()</decl.function.method.instance>
