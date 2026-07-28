// RUN: %target-swift-frontend -print-ast -module-name main %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

// CHECK-LABEL: enum MyError
@objc enum MyError: Int, Error {
  // CHECK: case somethingWrong
  case somethingWrong = 1
  // CHECK: case anotherIssue
  case anotherIssue = 2
  // CHECK: case terrible
  case terrible = 7

  // CHECK: internal static var _nsErrorDomain: String {
  // CHECK-NEXT:   get {
  // CHECK-NEXT:     return "main.MyError"
  // CHECK-NEXT:   }
  // CHECK-NEXT: }
}

struct ComplexSystem {
  // CHECK-LABEL: enum ComplexError
  @objc enum ComplexError: Int, Error {
    // CHECK: case internalError
    case internalError = 100
    // CHECK: case userError
    case userError = 101

    // CHECK: internal static var _nsErrorDomain: String {
    // CHECK-NEXT:   get {
    // CHECK-NEXT:     return "main.ComplexSystem.ComplexError"
    // CHECK-NEXT:   }
    // CHECK-NEXT: }
  }
}

// CHECK-LABEL: enum TransportError
@objc private enum TransportError: Int, Error {
  // CHECK: trafficJam
  case trafficJam = 1

  // CHECK: strike
  case strike = 17

  // CHECK: static var _nsErrorDomain: String {
  // CHECK-NEXT:   get {
  // CHECK-NEXT:     return String(reflecting: self)
  // CHECK-NEXT:   }
  // CHECK-NEXT: }
}
