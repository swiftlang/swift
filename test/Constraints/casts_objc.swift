// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
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

// rdar://problem/29496775 / SR-3319
func sr3319(f: CGFloat, n: NSNumber) {
  let _ = [f].map { $0 as NSNumber }
  let _ = [n].map { $0 as CGFloat }
}

func alwaysSucceedingConditionalCasts(f: CGFloat, n: NSNumber) {
  let _ = f as? NSNumber  // expected-warning{{conditional cast from 'CGFloat' to 'NSNumber' always succeeds}}
  let _ = n as? CGFloat  // expected-warning{{conditional cast from 'NSNumber' to 'CGFloat' always succeeds}}
}

func optionalityReducingCasts(f: CGFloat?, n: NSNumber?) {
  let _ = f as? NSNumber // expected-error{{downcast from 'CGFloat?' to 'NSNumber' only unwraps optionals; did you mean to use '!'?}}
  let _ = f as! NSNumber // expected-error{{downcast from 'CGFloat?' to 'NSNumber' only unwraps optionals; did you mean to use '!'?}}
  let _ = n as? CGFloat // expected-error{{downcast from 'NSNumber?' to 'CGFloat' only unwraps optionals; did you mean to use '!'?}}
  let _ = n as! CGFloat // expected-error{{downcast from 'NSNumber?' to 'CGFloat' only unwraps optionals; did you mean to use '!'?}}
}

func optionalityMatchingCasts(f: CGFloat?, n: NSNumber?) {
  let _ = f as NSNumber?
  let _ = f as? NSNumber? // expected-warning{{conditional cast from 'CGFloat?' to 'NSNumber?' always succeeds}}
  let _ = f as! NSNumber? // expected-warning{{forced cast from 'CGFloat?' to 'NSNumber?' always succeeds; did you mean to use 'as'?}}{{13-16=as}}
  let _ = n as CGFloat?
  let _ = n as? CGFloat? // expected-warning{{conditional cast from 'NSNumber?' to 'CGFloat?' always succeeds}}
  let _ = n as! CGFloat? // expected-warning{{forced cast from 'NSNumber?' to 'CGFloat?' always succeeds; did you mean to use 'as'?}}{{13-16=as}}
}

func optionalityMatchingCastsIUO(f: CGFloat?!, n: NSNumber?!) {
  let _ = f as NSNumber?
  let _ = f as? NSNumber? // expected-warning{{conditional cast from 'CGFloat?!' to 'NSNumber?' always succeeds}}
  let _ = f as! NSNumber? // expected-warning{{forced cast from 'CGFloat?!' to 'NSNumber?' always succeeds; did you mean to use 'as'?}}
  let _ = n as CGFloat?
  let _ = n as? CGFloat? // expected-warning{{conditional cast from 'NSNumber?!' to 'CGFloat?' always succeeds}}
  let _ = n as! CGFloat? // expected-warning{{forced cast from 'NSNumber?!' to 'CGFloat?' always succeeds; did you mean to use 'as'?}}
}

func optionalityMismatchingCasts(f: CGFloat???, n: NSNumber???) {
  let _ = f as NSNumber?? // expected-error{{cannot convert value of type 'CGFloat???' to type 'NSNumber??' in coercion}}
  let _ = f as NSNumber???? // expected-error{{cannot convert value of type 'CGFloat???' to type 'NSNumber????' in coercion}}
  let _ = n as CGFloat?? // expected-error{{cannot convert value of type 'NSNumber???' to type 'CGFloat??' in coercion}}
  let _ = n as CGFloat???? // expected-error{{cannot convert value of type 'NSNumber???' to type 'CGFloat????' in coercion}}
}

func anyObjectCasts(xo: [Int]?, xooo: [Int]???) {
  _ = xo as AnyObject
  _ = xo as AnyObject?
  _ = xooo as AnyObject??
  _ = xooo as AnyObject???
  _ = xooo as AnyObject???? // expected-error{{cannot convert value of type '[Int]???' to type 'AnyObject????' in coercion}}
}
