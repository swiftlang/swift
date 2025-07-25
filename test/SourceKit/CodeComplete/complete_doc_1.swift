protocol P {
  /// This is a doc comment of P.foo
  ///
  /// Do whatever.
  func foo()

  /// This is a doc comment of P.bar
  ///
  /// May have default information.
  func bar()
}

extension P {
  func bar() {}
}

struct S: P {
  func foo() {}
}

func test() {
  S().#^DOC^#
}

// All in main module.
// RUN: %target-swift-ide-test -code-completion -source-filename %s -module-name DocBriefTest -code-completion-token=DOC -code-completion-comments=true | %FileCheck %s -check-prefix=CHECK

// CHECK:      Decl[InstanceMethod]/CurrNominal:  foo()[#Void#]; name=foo();
// CHECK-SAME: briefcomment=This is a doc comment of P.foo;
// CHECK-SAME: fullcomment=<Function file="{{.*}}" line="5" column="8"><Name>foo()</Name><USR>s:12DocBriefTest1PP3fooyyF</USR><Declaration>func foo()</Declaration><CommentParts><Abstract><Para>This is a doc comment of P.foo</Para></Abstract><Discussion><Para>Do whatever.</Para><Note><Para>This documentation comment was inherited from <codeVoice>P</codeVoice>.</Para></Note></Discussion></CommentParts></Function>

// CHECK:      Decl[InstanceMethod]/Super:        bar()[#Void#]; name=bar();
// CHECK-SAME: briefcomment=This is a doc comment of P.bar;
// CHECK-SAME: fullcomment=<Function file="{{.*}}" line="10" column="8"><Name>bar()</Name><USR>s:12DocBriefTest1PP3baryyF</USR><Declaration>func bar()</Declaration><CommentParts><Abstract><Para>This is a doc comment of P.bar</Para></Abstract><Discussion><Para>May have default information.</Para><Note><Para>This documentation comment was inherited from <codeVoice>P</codeVoice>.</Para></Note></Discussion></CommentParts></Function>
