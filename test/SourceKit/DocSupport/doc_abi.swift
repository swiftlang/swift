// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -module-name Mod %t/mod.swift -o %t/Mod.swiftmodule
// RUN: %sourcekitd-test -req=doc-info -print-raw-response -module Mod -- -I %t -target %target-triple > %t/output.response
// RUN: %diff -u %t/expected.response %t/output.response

// Make sure we don't crash here, we should exclude the @abi decl from the
// list of entities.

//--- mod.swift
@abi(func bar())
public func foo() {}

@abi(var abiVar: Int)
public var apiVar: Int { 0 }

//--- expected.response
{
  key.annotations: [
    {
      key.kind: source.lang.swift.syntaxtype.attribute.builtin,
      key.offset: 0,
      key.length: 21
    },
    {
      key.kind: source.lang.swift.ref.struct,
      key.name: "Int",
      key.usr: "s:Si",
      key.offset: 17,
      key.length: 3
    },
    {
      key.kind: source.lang.swift.syntaxtype.keyword,
      key.offset: 22,
      key.length: 3
    },
    {
      key.kind: source.lang.swift.syntaxtype.identifier,
      key.offset: 26,
      key.length: 6
    },
    {
      key.kind: source.lang.swift.ref.struct,
      key.name: "Int",
      key.usr: "s:Si",
      key.offset: 34,
      key.length: 3
    },
    {
      key.kind: source.lang.swift.syntaxtype.keyword,
      key.offset: 40,
      key.length: 3
    },
    {
      key.kind: source.lang.swift.syntaxtype.attribute.builtin,
      key.offset: 47,
      key.length: 16
    },
    {
      key.kind: source.lang.swift.syntaxtype.keyword,
      key.offset: 64,
      key.length: 4
    },
    {
      key.kind: source.lang.swift.syntaxtype.identifier,
      key.offset: 69,
      key.length: 3
    }
  ],
  key.entities: [
    {
      key.kind: source.lang.swift.decl.var.global,
      key.name: "apiVar",
      key.usr: "s:3Mod6apiVarSivp",
      key.offset: 0,
      key.length: 45,
      key.fully_annotated_decl: "<decl.var.global><syntaxtype.attribute.builtin>@abi(<decl.var.global><syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>abiVar</decl.name>: <decl.var.type><ref.struct usr=\"s:Si\">Int</ref.struct></decl.var.type></decl.var.global>)</syntaxtype.attribute.builtin> <syntaxtype.keyword>var</syntaxtype.keyword> <decl.name>apiVar</decl.name>: <decl.var.type><ref.struct usr=\"s:Si\">Int</ref.struct></decl.var.type> { <syntaxtype.keyword>get</syntaxtype.keyword> }</decl.var.global>"
    },
    {
      key.kind: source.lang.swift.decl.function.free,
      key.name: "foo()",
      key.usr: "s:3Mod3fooyyF",
      key.offset: 47,
      key.length: 27,
      key.fully_annotated_decl: "<decl.function.free><syntaxtype.attribute.builtin>@abi(<decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>bar</decl.name>()</decl.function.free>)</syntaxtype.attribute.builtin> <syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>()</decl.function.free>"
    }
  ],
  key.sourcetext: "@abi(var abiVar: Int)\nvar apiVar: Int { get }\n\n@abi(func bar())\nfunc foo()\n\n"
}
