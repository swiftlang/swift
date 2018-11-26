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
let _: NSString! = unsafeDowncast(r22507759)  // expected-error {{missing argument for parameter 'to' in call}}

// rdar://problem/29496775 / SR-3319
func sr3319(f: CGFloat, n: NSNumber) {
  let _ = [f].map { $0 as NSNumber }
  let _ = [n].map { $0 as! CGFloat }
}

func alwaysSucceedingConditionalCasts(f: CGFloat, n: NSNumber) {
  let _ = f as? NSNumber  // expected-warning{{conditional cast from 'CGFloat' to 'NSNumber' always succeeds}}
  let _ = n as? CGFloat
}

func optionalityReducingCasts(f: CGFloat?, n: NSNumber?) {
  let _ = f as? NSNumber // expected-warning{{conditional downcast from 'CGFloat?' to 'NSNumber' is a bridging conversion; did you mean to use 'as'?}}
  let _ = f as! NSNumber // expected-warning{{forced cast from 'CGFloat?' to 'NSNumber' only unwraps and bridges; did you mean to use '!' with 'as'?}}
  let _ = n as? CGFloat
  let _ = n as! CGFloat
}

func optionalityMatchingCasts(f: CGFloat?, n: NSNumber?) {
  let _ = f as NSNumber?
  let _ = f as? NSNumber? // expected-warning{{conditional cast from 'CGFloat?' to 'NSNumber?' always succeeds}}
  let _ = f as! NSNumber? // expected-warning{{forced cast from 'CGFloat?' to 'NSNumber?' always succeeds; did you mean to use 'as'?}}{{13-16=as}}
  let _ = n as? CGFloat?
  let _ = n as! CGFloat?
}

func optionalityMatchingCastsIUO(f: CGFloat?!, n: NSNumber?!) {
  let _ = f as NSNumber?
  let _ = f as? NSNumber? // expected-warning{{conditional downcast from 'CGFloat??' to 'NSNumber?' is a bridging conversion; did you mean to use 'as'?}}
  let _ = f as! NSNumber? // expected-warning{{forced cast from 'CGFloat??' to 'NSNumber?' only unwraps and bridges; did you mean to use '!' with 'as'?}}
  let _ = n as? CGFloat?
  let _ = n as! CGFloat?
}

func optionalityMismatchingCasts(f: CGFloat, n: NSNumber, fooo: CGFloat???, 
                                 nooo: NSNumber???) {
  _ = f as NSNumber?
  _ = f as NSNumber??
  let _ = fooo as NSNumber?? // expected-error{{'CGFloat???' is not convertible to 'NSNumber??'; did you mean to use 'as!' to force downcast?}}
  let _ = fooo as NSNumber???? // okay: injects extra optionals
}

func anyObjectCasts(xo: [Int]?, xooo: [Int]???, x: [Int]) {
  _ = x as AnyObject
  _ = x as AnyObject?
  _ = xo as AnyObject
  _ = xo as AnyObject?
  _ = xooo as AnyObject??
  _ = xooo as AnyObject???
  _ = xooo as AnyObject???? // okay: injects extra optionals
}
