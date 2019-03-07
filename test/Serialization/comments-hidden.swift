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

/// PublicClass Documentation
public class PublicClass {
  /// Public Function Documentation
  public func f_public() { }
  /// Internal Function Documentation NotForNormal
  internal func f_internal() { }
  /// Private Function Documentation NotForNormal NotForTesting
  private func f_private() { }
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

// TESTING-NEGATIVE-NOT: NotForTesting
// TESTING: PublicClass Documentation
// TESTING: Public Function Documentation
// TESTING: Internal Function Documentation
// TESTING: InternalClass Documentation
// TESTING: Internal Function Documentation


