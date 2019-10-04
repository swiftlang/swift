// Test the case when we compile normally
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc %s
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -I %t > %t.normal.txt
// RUN: %FileCheck %s -check-prefix=NORMAL < %t.normal.txt
// RUN: %FileCheck %s -check-prefix=NORMAL-NEGATIVE < %t.normal.txt

// Test the case when we compile with -enable-testing
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-testing -module-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc %s
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -I %t > %t.testing.txt
// RUN: %FileCheck %s -check-prefix=TESTING < %t.testing.txt
// RUN: %FileCheck %s -check-prefix=TESTING-NEGATIVE < %t.testing.txt

// Test the case when we have .swiftsourceinfo
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-testing -module-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc -emit-module-source-info-path %t/comments.swiftsourceinfo %s
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -I %t > %t.testing.txt
// RUN: %FileCheck %s -check-prefix=SOURCE-LOC < %t.testing.txt

/// PublicClass Documentation
public class PublicClass {
  /// Public Function Documentation
  public func f_public() { }
  /// Public Init Documentation
  public init(_ name: String) {}
  /// Public Subscript Documentation
  public subscript(_ name: String) -> Int { return 0 }
  /// Internal Function Documentation NotForNormal
  internal func f_internal() { }
  /// Private Function Documentation NotForNormal NotForTesting
  private func f_private() { }
  /// Public Filter Function Documentation NotForNormal NotForTesting
  public func __UnderscoredPublic() {}
  /// Public Filter Init Documentation NotForNormal NotForTesting
  public init(__label name: String) {}
  /// Public Filter Subscript Documentation NotForNormal NotForFiltering
  public subscript(__label name: String) -> Int { return 0 }
  /// Public Filter Init Documentation NotForNormal NotForTesting
  public init(label __name: String) {}
  /// Public Filter Subscript Documentation NotForNormal NotForTesting
  public subscript(label __name: String) -> Int { return 0 }
}

public extension PublicClass {
  /// Public Filter Operator Documentation NotForNormal NotForTesting
  static func -=(__lhs: inout PublicClass, __rhs: PublicClass) {}
}

/// InternalClass Documentation NotForNormal
internal class InternalClass {
  /// Internal Function Documentation NotForNormal
  internal func f_internal() { }
  /// Private Function Documentation NotForNormal NotForTesting
  private func f_private() { }
}

/// PrivateClass Documentation NotForNormal NotForTesting
private class PrivateClass {
  /// Private Function Documentation NotForNormal NotForTesting
  private func f_private() { }
}

// NORMAL-NEGATIVE-NOT: NotForNormal
// NORMAL-NEGATIVE-NOT: NotForTesting
// NORMAL: PublicClass Documentation
// NORMAL: Public Function Documentation
// NORMAL: Public Init Documentation
// NORMAL: Public Subscript Documentation

// TESTING-NEGATIVE-NOT: NotForTesting
// TESTING: PublicClass Documentation
// TESTING: Public Function Documentation
// TESTINH: Public Init Documentation
// TESTING: Public Subscript Documentation
// TESTING: Internal Function Documentation
// TESTING: InternalClass Documentation
// TESTING: Internal Function Documentation

// SOURCE-LOC: comments-hidden.swift:37:15: Func/PublicClass.__UnderscoredPublic RawComment=none BriefComment=none DocCommentAsXML=none
// SOURCE-LOC: comments-hidden.swift:39:10: Constructor/PublicClass.init RawComment=none BriefComment=none DocCommentAsXML=none
// SOURCE-LOC: comments-hidden.swift:41:10: Subscript/PublicClass.subscript RawComment=none BriefComment=none DocCommentAsXML=none
// SOURCE-LOC: comments-hidden.swift:43:10: Constructor/PublicClass.init RawComment=none BriefComment=none DocCommentAsXML=none
// SOURCE-LOC: comments-hidden.swift:45:10: Subscript/PublicClass.subscript RawComment=none BriefComment=none DocCommentAsXML=none
// SOURCE-LOC: comments-hidden.swift:50:15: Func/-= RawComment=none BriefComment=none DocCommentAsXML=none
