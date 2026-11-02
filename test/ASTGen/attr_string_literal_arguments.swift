// Attribute string arguments are the text between the quotes, exactly as
// written: the interface printer cooks escapes, line continuations and the
// indentation of multi-line literals.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck %s -language-mode 5 -module-name M \
// RUN:   -enable-library-evolution -emit-module-interface-path %t/cpp.swiftinterface
// RUN: %target-swift-frontend -typecheck %s -language-mode 5 -module-name M \
// RUN:   -enable-library-evolution -emit-module-interface-path %t/astgen.swiftinterface \
// RUN:   -enable-experimental-feature ParserASTGen
// RUN: %FileCheck %s --input-file %t/cpp.swiftinterface
// RUN: %FileCheck %s --input-file %t/astgen.swiftinterface

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen

public struct S {
  // CHECK: message: "abc def"
  @available(swift, deprecated: 4.2, message: """
    abc \
    def
    """)
  public func continuation() {}

  // CHECK: message: "first\nsecond"
  @available(swift, deprecated: 4.2, message: """
    first
    second
    """)
  public func lines() {}

  // CHECK: message: "tab\there \"quoted\" A"
  @available(swift, deprecated: 4.2, message: "tab\there \"quoted\" \u{41}")
  public func escapes() {}

  // CHECK: renamed: "other(_:)"
  @available(*, deprecated, renamed: "other(_:)")
  public func renamed() {}

  // CHECK: @available(swift, deprecated: 4.2)
  @available(swift, deprecated: 4.2, message: "")
  public func empty() {}
}
