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
  S().
}

// All in main module.
// RUN: %sourcekitd-test -req=complete -req-opts=includefulldocumentation=1 -pos=22:7 %s -- %s -module-name DocBriefTest | %FileCheck %s -check-prefix=CHECK

// CHECK: {
// CHECK:   key.results: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-NEXT:       key.name: "bar()",
// CHECK-NEXT:       key.doc.full_as_xml: "<Function file=\"{{.*}}\" line=\"{{.*}}\" column=\"{{.*}}\"><Name>bar()</Name><USR>s:12DocBriefTest1PP3baryyF</USR><Declaration>func bar()</Declaration><CommentParts><Abstract><Para>This is a doc comment of P.bar</Para></Abstract><Discussion><Para>May have default information.</Para><Note><Para>This documentation comment was inherited from <codeVoice>P</codeVoice>.</Para></Note></Discussion></CommentParts></Function>",
// CHECK-NEXT:       key.description: "bar()",
// CHECK-NEXT:       key.typename: "Void",
// CHECK-NEXT:       key.doc.brief: "This is a doc comment of P.bar",
// CHECK-NEXT:       key.context: source.codecompletion.context.superclass,
// CHECK-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-NEXT:       key.num_bytes_to_erase: 0,
// CHECK-NEXT:       key.associated_usrs: "s:12DocBriefTest1PPAAE3baryyF",
// CHECK-NEXT:       key.modulename: "DocBriefTest",
// CHECK-NEXT:       key.sourcetext: "bar()"
// CHECK-NEXT:     },
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-NEXT:       key.name: "foo()",
// CHECK-NEXT:       key.doc.full_as_xml: "<Function file=\"{{.*}}\" line=\"{{.*}}\" column=\"{{.*}}\"><Name>foo()</Name><USR>s:12DocBriefTest1PP3fooyyF</USR><Declaration>func foo()</Declaration><CommentParts><Abstract><Para>This is a doc comment of P.foo</Para></Abstract><Discussion><Para>Do whatever.</Para><Note><Para>This documentation comment was inherited from <codeVoice>P</codeVoice>.</Para></Note></Discussion></CommentParts></Function>",
// CHECK-NEXT:       key.description: "foo()",
// CHECK-NEXT:       key.typename: "Void",
// CHECK-NEXT:       key.doc.brief: "This is a doc comment of P.foo",
// CHECK-NEXT:       key.context: source.codecompletion.context.thisclass,
// CHECK-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-NEXT:       key.num_bytes_to_erase: 0,
// CHECK-NEXT:       key.associated_usrs: "s:12DocBriefTest1SV3fooyyF s:12DocBriefTest1PP3fooyyF",
// CHECK-NEXT:       key.modulename: "DocBriefTest",
// CHECK-NEXT:       key.sourcetext: "foo()"
// CHECK-NEXT:     }
