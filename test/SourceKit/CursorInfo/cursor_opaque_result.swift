public protocol P {
  associatedtype Assoc
  func foo() -> Assoc
}
extension P {
  func bar() -> Assoc { fatalError() }
}

public struct MyStruct: P {
  public func foo() -> some Comparable { 1 }
}
func test(value: MyStruct) {
  value.foo()
  value.bar()
}

// RUN: %sourcekitd-test -req=cursor -pos=13:9 %s -- %s -module-name MyModule | %FileCheck --check-prefix=OPAQUE %s
// RUN: %sourcekitd-test -req=cursor -pos=14:9 %s -- %s -module-name MyModule | %FileCheck --check-prefix=ASSOC %s

// OPAQUE: foo()
// OPAQUE-NEXT: s:8MyModule0A6StructV3fooQryF
// OPAQUE-NEXT: (MyStruct) -> () -> some Comparable
// OPAQUE-NEXT: $sQrycD
// OPAQUE-NEXT: <Container>$s8MyModule0A6StructVD</Container>
// OPAQUE-NEXT: <Declaration>public func foo() -&gt; some <Type usr="s:SL">Comparable</Type></Declaration>
// OPAQUE-NEXT: <decl.function.method.instance><syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>() -&gt; <decl.function.returntype><syntaxtype.keyword>some</syntaxtype.keyword> <ref.protocol usr="s:SL">Comparable</ref.protocol></decl.function.returntype></decl.function.method.instance>


// ASSOC: bar()
// ASSOC-NEXT: s:8MyModule1PPAAE3bar5AssocQzyF
// ASSOC-NEXT: <Self where Self : P> (Self) -> () -> Self.Assoc
// ASSOC-NEXT: $s5AssocQzycD
// ASSOC-NEXT: <Container>$s8MyModule0A6StructVD</Container>
// ASSOC-NEXT: <Declaration>func bar() -&gt; <Type usr="s:8MyModule0A6StructV5Assoca">Assoc</Type></Declaration>
// ASSOC-NEXT: <decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>bar</decl.name>() -&gt; <decl.function.returntype><ref.typealias usr="s:8MyModule0A6StructV5Assoca">Assoc</ref.typealias></decl.function.returntype></decl.function.method.instance>
