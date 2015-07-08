// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s
// REQUIRES: objc_interop

import Foundation

struct NotClass {}

class SomeClass {}

func nsobject_as_class_cast<T>(x: NSObject, _: T) {
  let _ = x is AnyObject.Type
  let _ = x as! AnyObject.Type
  let _ = x as? AnyObject.Type

  let _ = x is Any.Type
  let _ = x as! Any.Type
  let _ = x as? Any.Type

  let _ = x is SomeClass.Type
  let _ = x as! SomeClass.Type
  let _ = x as? SomeClass.Type

  let _ = x is T.Type
  let _ = x as! T.Type
  let _ = x as? T.Type

  let _ = x is NotClass.Type  // expected-warning{{cast from 'NSObject' to unrelated type 'NotClass.Type' always fails}}
  let _ = x as! NotClass.Type // expected-warning{{cast from 'NSObject' to unrelated type 'NotClass.Type' always fails}}
  let _ = x as? NotClass.Type // expected-warning{{cast from 'NSObject' to unrelated type 'NotClass.Type' always fails}}
}


