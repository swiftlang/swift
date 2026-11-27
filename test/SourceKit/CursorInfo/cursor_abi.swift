@abi(var abiVar: Int)
public var apiVar: Int { 0 }

public struct S {
  @abi(var abiStored: Int)
  public var apiStored: Int
}

// RUN: %sourcekitd-test -req=cursor -pos=2:12 %s -- %s | %FileCheck -check-prefix=CHECK-VAR %s
// RUN: %sourcekitd-test -req=cursor -pos=6:14 %s -- %s | %FileCheck -check-prefix=CHECK-STORED %s

// CHECK-VAR: source.lang.swift.decl.var.global (2:12-2:18)
// CHECK-VAR: <Declaration>@abi(var abiVar: <Type usr="s:Si">Int</Type>) public var apiVar: <Type usr="s:Si">Int</Type> { get }</Declaration>
// CHECK-VAR: <decl.var.global><syntaxtype.attribute.builtin>@abi(<decl.var.global><syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>abiVar</decl.name>: <decl.var.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.type></decl.var.global>)</syntaxtype.attribute.builtin> <syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>apiVar</decl.name>: <decl.var.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.type> { <syntaxtype.keyword>get</syntaxtype.keyword> }</decl.var.global>

// CHECK-STORED: source.lang.swift.decl.var.instance (6:14-6:23)
// CHECK-STORED: <Declaration>@abi(var abiStored: <Type usr="s:Si">Int</Type>) public var apiStored: <Type usr="s:Si">Int</Type></Declaration>
// CHECK-STORED: <decl.var.instance><syntaxtype.attribute.builtin>@abi(<decl.var.instance><syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>abiStored</decl.name>: <decl.var.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.type></decl.var.instance>)</syntaxtype.attribute.builtin> <syntaxtype.keyword>public</syntaxtype.keyword> <syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>apiStored</decl.name>: <decl.var.type><ref.struct usr="s:Si">Int</ref.struct></decl.var.type></decl.var.instance>
