// RUN: %target-swift-ide-test -print-indexed-symbols -source-filename %s 2>&1 | %FileCheck -check-prefix CHECK %s

// https://github.com/apple/swift/issues/54900
//
// Test that we don't crash when validating members inside an extension with no
// type name.

// CHECK: :[[@LINE+1]]:11: error: expected type name in extension declaration
extension {
  // CHECK: :[[@LINE+1]]:8: error: operator '==' declared in extension must be 'static'
  func ==(lhs: Any, rhs: Any) -> Bool {}
}
