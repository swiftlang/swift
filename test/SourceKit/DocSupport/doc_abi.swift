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

//--- expected.response
{
  key.annotations: [
    {
      key.kind: source.lang.swift.syntaxtype.attribute.builtin,
      key.offset: 0,
      key.length: 16
    },
    {
      key.kind: source.lang.swift.syntaxtype.keyword,
      key.offset: 17,
      key.length: 4
    },
    {
      key.kind: source.lang.swift.syntaxtype.identifier,
      key.offset: 22,
      key.length: 3
    }
  ],
  key.entities: [
    {
      key.kind: source.lang.swift.decl.function.free,
      key.name: "foo()",
      key.usr: "s:3Mod3fooyyF",
      key.offset: 0,
      key.length: 27,
      key.fully_annotated_decl: "<decl.function.free><syntaxtype.attribute.builtin>@abi(<decl.function.free><syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>bar</decl.name>()</decl.function.free>)</syntaxtype.attribute.builtin> <syntaxtype.keyword>func</syntaxtype.keyword> <decl.name>foo</decl.name>()</decl.function.free>"
    }
  ],
  key.sourcetext: "@abi(func bar())\nfunc foo()\n\n"
}
