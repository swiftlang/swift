
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -emit-ir -g -verify

// REQUIRES: objc_interop

import Swift
import Foundation
import objc_generics

extension GenericClass {
  func method() {}
  class func classMethod() {}
}
