// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify %s
// REQUIRES: objc_interop

import Foundation

struct NotClass {}

class SomeClass {}

func nsobject_as_class_cast<T>(_ x: NSObject, _: T) {
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


// <rdar://problem/20294245> QoI: Error message mentions value rather than key for subscript
func test(_ a : CFString!, b : CFString) {
  let dict = NSMutableDictionary()
  let object = NSObject()
  dict[a] = object


  dict[b] = object
}


// <rdar://problem/22507759> QoI: poor error message for invalid unsafeDowncast()
let r22507759: NSObject! = "test" as NSString
let _: NSString! = unsafeDowncast(r22507759)  // expected-error {{generic parameter 'T' could not be inferred}}

