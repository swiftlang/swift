// BEGIN Module.swift
public protocol P {
  /// This is a doc comment of P.foo
  ///
  /// Do whatever.
  func foo()
}

public struct S: P {
  public init() {}
  public func foo() {}
}

// BEGIN User.swift
import DocBriefTest
func test() {
  S().foo()
}

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name DocBriefTest \
// RUN:     -emit-module-path %t/Modules/DocBriefTest.swiftmodule \
// RUN:     -emit-module-doc-path %t/Modules/DocBriefTest.swiftdoc \
// RUN:     %t/Module.swift

// RUN: %sourcekitd-test -req=complete -pos=3:7 %t/User.swift -- %t/User.swift -I %t/Modules -target %target-triple -module-name DocBriefUser | %FileCheck %s -check-prefix=CHECK

// CHECK: {
// CHECK:   key.results: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.decl.function.method.instance,
// CHECK-NEXT:       key.name: "foo()",
// CHECK-NEXT:       key.sourcetext: "foo()",
// CHECK-NEXT:       key.description: "foo()",
// CHECK-NEXT:       key.typename: "Void",
// CHECK-NEXT:       key.doc.brief: "This is a doc comment of P.foo",
// CHECK-NEXT:       key.context: source.codecompletion.context.thisclass,
// CHECK-NEXT:       key.typerelation: source.codecompletion.typerelation.unknown,
// CHECK-NEXT:       key.num_bytes_to_erase: 0,
// CHECK-NEXT:       key.associated_usrs: "s:12DocBriefTest1SV3fooyyF s:12DocBriefTest1PP3fooyyF",
// CHECK-NEXT:       key.modulename: "DocBriefTest"
// CHECK-NEXT:     }

