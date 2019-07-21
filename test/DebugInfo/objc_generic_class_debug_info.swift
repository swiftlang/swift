
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -emit-ir -g -verify

// REQUIRES: objc_interop

import Swift
import Foundation
import objc_generics

public extension GenericClass {
  @objc func method() {}
  @objc class func classMethod() {}
}

public func takesFunction<T : AnyObject>(fn: @escaping (GenericClass<T>) -> ()) -> (GenericClass<T>) -> () {
  let copy = fn
  return copy
}
