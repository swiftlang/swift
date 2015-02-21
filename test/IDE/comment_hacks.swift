//===--- Check that we convert comments to XML correctly.

// RUN: %target-swift-frontend -parse -verify -disable-objc-attr-requires-foundation-module %s
// RUN: %target-swift-ide-test -print-comments -rest-temporary-hacks=true -source-filename %s -comments-xml-schema %S/../../bindings/xml/comment-xml-schema.rng > %t.txt
// RUN: FileCheck %s < %t.txt
// RUN: FileCheck %s -check-prefix=WRONG < %t.txt

// WRONG-NOT: CommentXMLInvalid

class A260_VerbatimHack {
  /// Aaa.
  ///
  ///  Bbb.
  ///
  ///  Ccc.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A260_VerbatimHack.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test17A260_VerbatimHack2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>Aaa.</Para></Abstract><Discussion><Verbatim kind="code" xml:space="preserve">Bbb.Ccc.</Verbatim></Discussion></Function>]
}

class A270_BackslashSpace {
  /// a string of `X`\ s.
  func f0() {}
// CHECK: swift:[[@LINE-1]]:8: Func/A270_BackslashSpace.f0 {{.*}} FullCommentAsXML=[<Function file="{{[^"]+}}swift" line="[[@LINE-1]]" column="8"><Name>f0()</Name><USR>s:FC14swift_ide_test19A270_BackslashSpace2f0FS0_FT_T_</USR><Declaration>func f0()</Declaration><Abstract><Para>a string of <rawHTML><![CDATA[<code>]]></rawHTML>X<rawHTML><![CDATA[</code>]]></rawHTML>s.</Para></Abstract></Function>]
}

