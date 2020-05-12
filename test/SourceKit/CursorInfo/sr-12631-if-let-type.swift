let foo: Int? = 123
if let bar: Int = foo {
  _ = bar
}

// RUN: %sourcekitd-test -req=cursor -pos=2:8 %s -- %s | %FileCheck --check-prefix=CHECK-BAR %s
// RUN: %sourcekitd-test -req=cursor -pos=3:7 %s -- %s | %FileCheck --check-prefix=CHECK-BAR %s
// RUN: %sourcekitd-test -req=cursor -pos=1:5 %s -- %s | %FileCheck --check-prefix=CHECK-FOO %s

// CHECK-BAR:      <Declaration>let bar: <Type usr="s:Si">Int</Type></Declaration>
// CHECK-BAR-NEXT: <decl.var.local><syntaxtype.keyword>let</syntaxtype.keyword> <decl.name>bar</decl.name>: <decl.var.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.type></decl.var.local>

// CHECK-FOO:      <Declaration>let foo: <Type usr="s:Si">Int</Type>?</Declaration>
// CHECK-FOO-NEXT: <decl.var.global><syntaxtype.keyword>let</syntaxtype.keyword> <decl.name>foo</decl.name>: <decl.var.type><ref.struct usr="s:Si">Int</ref.struct>?</decl.var.type></decl.var.global> 