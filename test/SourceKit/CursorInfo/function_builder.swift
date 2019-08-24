struct Tagged<Tag, Entity> {
  let tag: Tag
  let entity: Entity
}

protocol Taggable {
}

extension Taggable {
  func tag<Tag>(_ tag: Tag) -> Tagged<Tag, Self> {
    return Tagged(tag: tag, entity: self)
  }
}

extension Int: Taggable { }
extension String: Taggable { }

@_functionBuilder
struct TaggedBuilder<Tag> {
  static func buildBlock() -> () { }

  static func buildBlock<T1>(_ t1: Tagged<Tag, T1>) -> Tagged<Tag, T1> {
    return t1
  }

  static func buildBlock<T1, T2>(_ t1: Tagged<Tag, T1>, _ t2: Tagged<Tag, T2>) -> (Tagged<Tag, T1>, Tagged<Tag, T2>) {
    return (t1, t2)
  }
}

enum Color {
  case red, green, blue
}

func acceptColorTagged<Result>(@TaggedBuilder<Color> body: (Color) -> Result) {
  print(body(.blue))
}

func testAcceptColorTagged(i: Int, s: String) {
  acceptColorTagged { color in
    i.tag(color)
    s.tag(Color.green)
  }
}

// Custom attribute name.
// RUN: %sourcekitd-test -req=cursor -pos=35:33 %s -- %s -module-name BuilderTest | %FileCheck %s --check-prefix=ATTR_NAME
// ATTR_NAME: source.lang.swift.ref.struct (19:8-19:21)
// ATTR_NAME-NEXT: TaggedBuilder
// ATTR_NAME-NEXT: s:11BuilderTest06TaggedA0V
// ATTR_NAME-NEXT: TaggedBuilder<Tag>.Type
// ATTR_NAME-NEXT: $s11BuilderTest06TaggedA0VyxGmD
// ATTR_NAME-NEXT: <Declaration>@_functionBuilder struct TaggedBuilder&lt;Tag&gt;</Declaration>
// ATTR_NAME-NEXT: <decl.struct><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@_functionBuilder</syntaxtype.attribute.name></syntaxtype.attribute.builtin> <syntaxtype.keyword>struct</syntaxtype.keyword> <decl.name>TaggedBuilder</decl.name>&lt;<decl.generic_type_param usr="s:11BuilderTest06TaggedA0V3Tagxmfp"><decl.generic_type_param.name>Tag</decl.generic_type_param.name></decl.generic_type_param>&gt;</decl.struct>

// Generic argument in attribute name.
// RUN: %sourcekitd-test -req=cursor -pos=35:47 %s -- %s -module-name BuilderTest | %FileCheck %s --check-prefix=ATTR_GENERICARG
// ATTR_GENERICARG: source.lang.swift.ref.enum (31:6-31:11)
// ATTR_GENERICARG-NEXT: Color
// ATTR_GENERICARG-NEXT: s:11BuilderTest5ColorO
// ATTR_GENERICARG-NEXT: Color.Type
// ATTR_GENERICARG-NEXT: $s11BuilderTest5ColorOmD
// ATTR_GENERICARG-NEXT: <Declaration>enum Color</Declaration>
// ATTR_GENERICARG-NEXT: <decl.enum><syntaxtype.keyword>enum</syntaxtype.keyword> <decl.name>Color</decl.name></decl.enum>

// Call for function with builder.
// RUN: %sourcekitd-test -req=cursor -pos=40:3 %s -- %s -module-name BuilderTest | %FileCheck %s --check-prefix=CALL_BUILDERFUNC
// CALL_BUILDERFUNC: source.lang.swift.ref.function.free (35:6-35:78)
// CALL_BUILDERFUNC-NEXT: acceptColorTagged(body:)
// CALL_BUILDERFUNC-NEXT: s:11BuilderTest17acceptColorTagged4bodyyxAA0D0OXE_tlF
// CALL_BUILDERFUNC-NEXT: <Result> (body: (Color) -> Result) -> ()
// CALL_BUILDERFUNC-NEXT: $s4bodyyx11BuilderTest5ColorOXE_tcluD
// CALL_BUILDERFUNC-NEXT: <Declaration>func acceptColorTagged&lt;Result&gt;(@<Type usr="s:11BuilderTest06TaggedA0V">TaggedBuilder</Type>&lt;<Type usr="s:11BuilderTest5ColorO">Color</Type>&gt; body: (<Type usr="s:11BuilderTest5ColorO">Color</Type>) -&gt; <Type usr="s:11BuilderTest17acceptColorTagged4bodyyxAA0D0OXE_tlF6ResultL_xmfp">Result</Type>)</Declaration>
// CALL_BUILDERFUNC-NEXT: <decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>acceptColorTagged</decl.name>&lt;<decl.generic_type_param usr="s:11BuilderTest17acceptColorTagged4bodyyxAA0D0OXE_tlF6ResultL_xmfp"><decl.generic_type_param.name>Result</decl.generic_type_param.name></decl.generic_type_param>&gt;(<decl.var.parameter><syntaxtype.attribute.builtin><syntaxtype.attribute.name>@</syntaxtype.attribute.name><ref.struct usr="s:11BuilderTest06TaggedA0V">TaggedBuilder</ref.struct>&lt;<ref.enum usr="s:11BuilderTest5ColorO">Color</ref.enum>&gt;</syntaxtype.attribute.builtin> <decl.var.parameter.argument_label>body</decl.var.parameter.argument_label>: <decl.var.parameter.type>(<decl.var.parameter><decl.var.parameter.type><ref.enum usr="s:11BuilderTest5ColorO">Color</ref.enum></decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><ref.generic_type_param usr="s:11BuilderTest17acceptColorTagged4bodyyxAA0D0OXE_tlF6ResultL_xmfp">Result</ref.generic_type_param></decl.function.returntype></decl.var.parameter.type></decl.var.parameter>)</decl.function.free>

// Closure parameter - decl-site.
// RUN: %sourcekitd-test -req=cursor -pos=40:23 %s -- %s -module-name BuilderTest | %FileCheck %s --check-prefix=CLOSUREPARAM_DECL
// CLOSUREPARAM_DECL: source.lang.swift.decl.var.parameter (40:23-40:28)
// CLOSUREPARAM_DECL-NEXT: color
// CLOSUREPARAM_DECL-NEXT: s:11BuilderTest21testAcceptColorTagged1i1sySi_SStFAA0F0VyAA0E0OSiG_AFyAHSSGtAHXEfU_5colorL_AHvp
// CLOSUREPARAM_DECL-NEXT: Color
// CLOSUREPARAM_DECL-NEXT: $s11BuilderTest5ColorOD
// CLOSUREPARAM_DECL-NEXT: <Declaration>let color: <Type usr="s:11BuilderTest5ColorO">Color</Type></Declaration>
// CLOSUREPARAM_DECL-NEXT: <decl.var.parameter><syntaxtype.keyword>let</syntaxtype.keyword> <decl.var.parameter.name>color</decl.var.parameter.name>: <decl.var.parameter.type><ref.enum usr="s:11BuilderTest5ColorO">Color</ref.enum></decl.var.parameter.type></decl.var.parameter>

// Closure parameter - use-site.
// RUN: %sourcekitd-test -req=cursor -pos=41:11 %s -- %s -module-name BuilderTest | %FileCheck %s --check-prefix=CLOSUREPARAM_USER
// CLOSUREPARAM_USER: source.lang.swift.ref.var.local (40:23-40:28)
// CLOSUREPARAM_USER-NEXT: color
// CLOSUREPARAM_USER-NEXT: s:11BuilderTest21testAcceptColorTagged1i1sySi_SStFAA0F0VyAA0E0OSiG_AFyAHSSGtAHXEfU_5colorL_AHvp
// CLOSUREPARAM_USER-NEXT: Color
// CLOSUREPARAM_USER-NEXT: $s11BuilderTest5ColorOD
// CLOSUREPARAM_USER-NEXT: <Declaration>let color: <Type usr="s:11BuilderTest5ColorO">Color</Type></Declaration>
// CLOSUREPARAM_USER-NEXT: <decl.var.parameter><syntaxtype.keyword>let</syntaxtype.keyword> <decl.var.parameter.name>color</decl.var.parameter.name>: <decl.var.parameter.type><ref.enum usr="s:11BuilderTest5ColorO">Color</ref.enum></decl.var.parameter.type></decl.var.parameter>

// Captured variable.
// RUN: %sourcekitd-test -req=cursor -pos=41:5 %s -- %s -module-name BuilderTest | %FileCheck %s --check-prefix=CAPTURED_VALUE
// CAPTURED_VALUE: source.lang.swift.ref.var.local (39:28-39:29)
// CAPTURED_VALUE-NEXT: i
// CAPTURED_VALUE-NEXT: s:11BuilderTest21testAcceptColorTagged1i1sySi_SStFACL_Sivp
// CAPTURED_VALUE-NEXT: Int
// CAPTURED_VALUE-NEXT: $sSiD
// CAPTURED_VALUE-NEXT: <Declaration>let i: <Type usr="s:Si">Int</Type></Declaration>
// CAPTURED_VALUE-NEXT: <decl.var.parameter><syntaxtype.keyword>let</syntaxtype.keyword> <decl.var.parameter.name>i</decl.var.parameter.name>: <decl.var.parameter.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.parameter.type></decl.var.parameter>

// Method call to captured variable.
// RUN: %sourcekitd-test -req=cursor -pos=41:7 %s -- %s -module-name BuilderTest | %FileCheck %s --check-prefix=CAPTURED_VALUE_METHOD
// CAPTURED_VALUE_METHOD: source.lang.swift.ref.function.method.instance (10:8-10:28)
// CAPTURED_VALUE_METHOD: tag(_:)
// CAPTURED_VALUE_METHOD: s:11BuilderTest8TaggablePAAE3tagyAA6TaggedVyqd__xGqd__lF
// CAPTURED_VALUE_METHOD: <Self, Tag where Self : Taggable> (Self) -> (Tag) -> Tagged<Tag, Self>
// CAPTURED_VALUE_METHOD: $sy11BuilderTest6TaggedVyqd__xGqd__cluD
// CAPTURED_VALUE_METHOD: <Container>$sSiD</Container>
// CAPTURED_VALUE_METHOD: <Declaration>func tag&lt;Tag&gt;(_ tag: Tag) -&gt; <Type usr="s:11BuilderTest6TaggedV">Tagged</Type>&lt;Tag, <Type usr="s:Si">Int</Type>&gt;</Declaration>
// CAPTURED_VALUE_METHOD: <decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>tag</decl.name>&lt;Tag&gt;(<decl.var.parameter><decl.var.parameter.argument_label>_</decl.var.parameter.argument_label> <decl.var.parameter.name>tag</decl.var.parameter.name>: <decl.var.parameter.type>Tag</decl.var.parameter.type></decl.var.parameter>) -&gt; <decl.function.returntype><ref.struct usr="s:11BuilderTest6TaggedV">Tagged</ref.struct>&lt;Tag, <ref.struct usr="s:Si">Int</ref.struct>&gt;</decl.function.returntype></decl.function.method.instance>
