// RUN: %target-swift-frontend -print-ast -module-name main %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-NORMAL %s
// RUN: %target-swift-frontend -print-ast -load-plugin-library %swift-plugin-dir/%target-library-name(SwiftMacros) -enable-experimental-feature DeriveConformancesViaMacros -module-name main %s | %FileCheck --check-prefix=CHECK --check-prefix=CHECK-MACRO %s
// REQUIRES: objc_interop
// REQUIRES: swift_feature_DeriveConformancesViaMacros

import Foundation

// CHECK-LABEL: enum MyError
@objc enum MyError: Int, Error {
  // CHECK: internal static var _nsErrorDomain: String {
  // CHECK-NEXT:   get {
  // CHECK-NEXT:     return "main.MyError"
  // CHECK-NEXT:   }
  // CHECK-NEXT: }

  case somethingWrong = 1
  case anotherIssue = 2
  case terrible = 7
}

struct ComplexSystem {
  // CHECK-LABEL: enum ComplexError
  @objc enum ComplexError: Int, Error {
    // CHECK: internal static var _nsErrorDomain: String {
    // CHECK-NEXT:   get {
    // CHECK-NEXT:     return "main.ComplexSystem.ComplexError"
    // CHECK-NEXT:   }
    // CHECK-NEXT: }
    
    case internalError = 100
    case userError = 101
  }
}

// CHECK-LABEL: enum TransportError
@objc private enum TransportError: Int, Error {
  // CHECK:      static var _nsErrorDomain: String {
  // CHECK-NEXT:          get {
  // CHECK-NORMAL-NEXT:     return String(reflecting: self)
  // CHECK-MACRO-NEXT:      return Swift::String(reflecting: self)
  // CHECK-NEXT:          }
  // CHECK-NEXT:        }

  case trafficJam = 1
  case strike = 17
}
