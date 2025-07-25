// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name DocBriefTest \
// RUN:     -emit-module-path %t/Modules/DocBriefTest.swiftmodule \
// RUN:     -emit-module-doc-path %t/Modules/DocBriefTest.swiftdoc \
// RUN:     %t/Module.swift

//--- Module.swift
public protocol P {
  /// This is a doc comment of P.foo
  ///
  /// Do whatever.
  func foo()
}

//--- User.swift
import DocBriefTest
struct S: P {
  func foo() {}
}

func test() {
  // RUN: %sourcekitd-test -req=complete -pos=%(line+1):7 %t/User.swift -- %t/User.swift -I %t/Modules -target %target-triple -module-name DocBriefUser | %FileCheck %s -check-prefix=CHECK
  S().

  // CHECK: {
  // CHECK:   key.results: [
  // CHECK-NEXT:     {
  // CHECK-NEXT:       key.kind: source.lang.swift.decl.function.method.instance,
  // CHECK-NEXT:       key.name: "foo()",
  // CHECK-NEXT:       key.description: "foo()",
  // CHECK-NEXT:       key.typename: "Void",
  // CHECK-NEXT:       key.doc.brief: "This is a doc comment of P.foo",
  // CHECK-NEXT:       key.context: source.codecompletion.context.thisclass,
  // CHECK-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
  // CHECK-NEXT:       key.num_bytes_to_erase: 0,
  // CHECK-NEXT:       key.associated_usrs: "s:12DocBriefUser1SV3fooyyF s:12DocBriefTest1PP3fooyyF",
  // CHECK-NEXT:       key.modulename: "DocBriefUser",
  // CHECK-NEXT:       key.sourcetext: "foo()"
  // CHECK-NEXT:     }
}
