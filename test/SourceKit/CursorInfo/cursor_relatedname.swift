struct S {
    mutating func foo(x: Int) {}
    __consuming func foo(x: String) {}
}

// RUN: %sourcekitd-test -req=cursor -pos=2:19 %s -- %s -module-name MyMod | %FileCheck -check-prefix=CHECK1 %s

// CHECK1: source.lang.swift.decl.function.method.instance (2:19-2:30)
// CHECK1: foo(x:)
// CHECK1: s:5MyMod1SV3foo1xySi_tF
// CHECK1: source.lang.swift
// CHECK1: (inout S) -> (Int) -> ()
// CHECK1: $s1xySi_tcD
// CHECK1: <Declaration>mutating func foo(x: <Type usr="s:Si">Int</Type>)</Declaration>
// CHECK1: <decl.function.method.instance><syntaxtype.keyword>mutating</syntaxtype.keyword> <syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>x</decl.var.parameter.argument_label>: <decl.var.parameter.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.parameter.type></decl.var.parameter>)</decl.function.method.instance>
// CHECK1: RELATED BEGIN
// CHECK1: <RelatedName usr="s:5MyMod1SV3foo1xySS_tF">foo(x: String)</RelatedName>
// CHECK1: RELATED END

// RUN: %sourcekitd-test -req=cursor -pos=3:22 %s -- %s -module-name MyMod | %FileCheck -check-prefix=CHECK2 %s
 
// CHECK2: source.lang.swift.decl.function.method.instance (3:22-3:36)
// CHECK2: foo(x:)
// CHECK2: s:5MyMod1SV3foo1xySS_tF
// CHECK2: source.lang.swift
// CHECK2: (__owned S) -> (String) -> ()
// CHECK2: $s1xySS_tcD
// CHECK2: <Declaration>func foo(x: <Type usr="s:SS">String</Type>)</Declaration>
// CHECK2: <decl.function.method.instance><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>(<decl.var.parameter><decl.var.parameter.argument_label>x</decl.var.parameter.argument_label>: <decl.var.parameter.type><ref.struct usr="s:SS">String</ref.struct></decl.var.parameter.type></decl.var.parameter>)</decl.function.method.instance>
// CHECK2: RELATED BEGIN
// CHECK2: <RelatedName usr="s:5MyMod1SV3foo1xySi_tF">foo(x: Int)</RelatedName>
// CHECK2: RELATED END
