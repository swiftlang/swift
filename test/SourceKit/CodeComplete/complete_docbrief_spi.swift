// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name DocBriefTest \
// RUN:     -emit-module-path %t/Modules/DocBriefTest.swiftmodule \
// RUN:     -emit-module-source-info-path %t/Modules/DocBriefTest.swiftsourceinfo \
// RUN:     %t/Module.swift

//--- Module.swift
@_spi(SomeSPI)
public protocol P {
  /// This is a doc comment of P.foo
  ///
  /// Do whatever.
  func foo()
}

@_spi(SomeSPI)
public struct S: P {
  public init() {}
  public func foo() {}
}

//--- User.swift
@_spi(SomeSPI) import DocBriefTest

func test() {
  // RUN: %sourcekitd-test -req=complete -pos=%(line+1):7 %t/User.swift -- %t/User.swift -I %t/Modules -target %target-triple -module-name DocBriefUser | %FileCheck %s -check-prefix=CHECK
  S().foo()

  // CHECK: {
  // CHECK:   key.results: [
  // CHECK-NEXT:     {
  // CHECK-NEXT:       key.kind: source.lang.swift.decl.function.method.instance,
  // CHECK-NEXT:       key.name: "foo()",
  // CHECK-NEXT:       key.description: "foo()",
  // CHECK-NEXT:       key.typename: "Void",
  // CHECK-NEXT:       key.doc.brief: "This is a doc comment of P.foo",
  // CHECK:            key.sourcetext: "foo()"
  // CHECK:          }
}
