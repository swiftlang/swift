// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Modules)
// RUN: split-file --leading-lines %s %t

// RUN: %target-swift-frontend \
// RUN:     -emit-module \
// RUN:     -module-name DocBriefTest \
// RUN:     -package-name DocPackage \
// RUN:     -emit-module-path %t/Modules/DocBriefTest.swiftmodule \
// RUN:     -emit-module-source-info-path %t/Modules/DocBriefTest.swiftsourceinfo \
// RUN:     %t/Module.swift

//--- Module.swift
package protocol P {
  /// This is a doc comment of P.foo
  ///
  /// Do whatever.
  func foo()
}

package struct S: P {
  public init() {}
  public func foo() {}
}

//--- User.swift
package import DocBriefTest

func test() {
  // RUN: %target-swift-ide-test -code-completion -source-filename %t/User.swift -I %t/Modules -target %target-triple -module-name DocBriefUser -package-name DocPackage -code-completion-token=DOC -code-completion-comments=true | %FileCheck %s -check-prefix=CHECK
  S().#^DOC^#foo()

  // CHECK:      Decl[InstanceMethod]/CurrNominal: foo()[#Void#]; name=foo();
  // CHECK-SAME: briefcomment=This is a doc comment of P.foo;
  // CHECK-SAME: fullcomment=<Function file="{{.*}}" line="18" column="8"><Name>foo()</Name><USR>s:12DocBriefTest1PP3fooyyF</USR><Declaration>func foo()</Declaration><CommentParts><Abstract><Para>This is a doc comment of P.foo</Para></Abstract><Discussion><Para>Do whatever.</Para><Note><Para>This documentation comment was inherited from <codeVoice>P</codeVoice>.</Para></Note></Discussion></CommentParts></Function>
}
