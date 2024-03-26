// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

// REQUIRES: objc_interop

import CoreFoundation
import Foundation

func testCFToObjC(_ cfStr: CFString, cfMutableStr: CFMutableString) {
  var nsStr: NSString = cfStr
  nsStr = cfMutableStr
  _ = nsStr

  var nsMutableStr: NSMutableString = cfMutableStr
  nsMutableStr = cfStr // expected-error{{cannot assign value of type 'CFString' to type 'NSMutableString'}}

  // soundness check
  nsStr = nsMutableStr
}

func testObjCToCF(_ nsStr: NSString, nsMutableStr: NSMutableString) {
  var cfStr: CFString = nsStr
  cfStr = nsMutableStr

  var cfMutableStr: CFMutableString = nsMutableStr
  cfMutableStr = cfStr // expected-error{{cannot assign value of type 'CFString' to type 'CFMutableString'}}

  // soundness check
  cfStr = cfMutableStr
}

func testCFToNative(_ cfStr: CFString, cfMutableStr: CFMutableString) {
  var str = cfStr as String
  str = cfMutableStr as String
  _ = str
}

func testNativeToCF(_ str: String) {
  var cfStr = str as CFString
  var cfMutableStr = str as CFMutableString // expected-error{{'String' is not convertible to 'CFMutableString'}} 
  // expected-note@-1{{did you mean to use 'as!' to force downcast?}} {{26-28=as!}}
}

func testCFToAnyObject(_ cfStr: CFString, cfMutableStr: CFMutableString,
                       cfTree: CFTree) {
  var anyObject: AnyObject = cfStr
  anyObject = cfMutableStr
  anyObject = cfTree
  _ = anyObject
}

func testAnyObjectToCF(_ anyObject: AnyObject) {
  var cfStr: CFString = anyObject as! CFString
  var _: CFMutableString = anyObject as! CFMutableString
  var _: CFTree = anyObject as! CFTree

  // No implicit conversions.
  cfStr = anyObject // expected-error{{'AnyObject' is not convertible to 'CFString'}} 
  // expected-note@-1{{did you mean to use 'as!' to force downcast?}} {{20-20= as! CFString}}
  _ = cfStr
}

func testUncheckableCasts(_ anyObject: AnyObject, nsObject: NSObject,
                          anyObjectType: AnyObject.Type, 
                          nsObjectType: NSObject.Type) {
  if let _ = anyObject as? CFString { } // expected-error{{conditional downcast to CoreFoundation type 'CFString' will always succeed}} expected-note{{did you mean to explicitly compare the CFTypeIDs of 'anyObject' and 'CFString'}}
  if let _ = nsObject as? CFString { } // expected-error{{conditional downcast to CoreFoundation type 'CFString' will always succeed}} expected-note{{did you mean to explicitly compare the CFTypeIDs of 'nsObject' and 'CFString'}}

  if let _ = anyObject as? CFTree { } // expected-error{{conditional downcast to CoreFoundation type 'CFTree' will always succeed}} expected-note{{did you mean to explicitly compare the CFTypeIDs of 'anyObject' and 'CFTree'}}
  if let _ = nsObject as? CFTree { } // expected-error{{will always succeed}} expected-note{{did you mean to explicitly compare the CFTypeIDs of 'nsObject' and 'CFTree'}}

  if let _ = anyObjectType as? CFString.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFString.Type' will always succeed}} expected-note{{did you mean to explicitly compare the CFTypeIDs of 'anyObjectType' and 'CFString.Type'}}
  if let _ = nsObjectType as? CFString.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFString.Type' will always succeed}} expected-note{{did you mean to explicitly compare the CFTypeIDs of 'nsObjectType' and 'CFString.Type'}}

  if let _ = anyObjectType as? CFTree.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFTree.Type' will always succeed}} expected-note{{did you mean to explicitly compare the CFTypeIDs of 'anyObjectType' and 'CFTree.Type'}}
  if let _ = nsObjectType as? CFTree.Type { } // expected-error{{will always succeed}} expected-note{{did you mean to explicitly compare the CFTypeIDs of 'nsObjectType' and 'CFTree.Type'}}

}

func testCFConvWithIUO(_ x: CFString!, y: NSString!) {
  func acceptCFString(_ a: CFString!) { }
  func acceptNSString(_ b: NSString!) { }

  acceptNSString(x)
  acceptCFString(y)
}

func testBridgedCFDowncast(array: [Any], dictionary: [AnyHashable : Any], set: Set<AnyHashable>) {
  let cfArray = array as CFArray
  let cfDictionary = dictionary as CFDictionary
  let cfSet = set as CFSet

  _ = array as? CFArray // expected-warning {{conditional cast from '[Any]' to 'CFArray' always succeeds}}
  _ = dictionary as? CFDictionary // expected-warning {{conditional cast from '[AnyHashable : Any]' to 'CFDictionary' always succeeds}}
  _ = set as? CFSet // expected-warning {{conditional cast from 'Set<AnyHashable>' to 'CFSet' always succeeds}}

  _ = array as! CFArray // expected-warning {{forced cast from '[Any]' to 'CFArray' always succeeds}}
  _ = dictionary as! CFDictionary // expected-warning {{forced cast from '[AnyHashable : Any]' to 'CFDictionary' always succeeds}}
  _ = set as! CFSet // expected-warning {{forced cast from 'Set<AnyHashable>' to 'CFSet' always succeeds}}

  _ = cfArray as! [Any]
  _ = cfDictionary as! [AnyHashable : Any]
  _ = cfSet as! Set<AnyHashable>

  _ = cfArray as? [Any]
  _ = cfDictionary as? [AnyHashable : Any]
  _ = cfSet as? Set<AnyHashable>
}

func testCastWithImplicitErasure() {
  enum Info {
    var id: String { "" }
    var options: [CFString : Any]? { nil }
  }

  class Null {}

  struct Test {
    var flag: Bool = false
    var info: Info

    func test(key1: CFString!, key2: CFString!, key3: CFString) -> CFDictionary {
      [
        key1: flag,
        key2: info.id,
        key3: info.options ?? Null()
        // expected-warning@-1 {{expression implicitly coerced from 'Any?' to 'Any'}}
        // expected-note@-2 {{provide a default value to avoid this warning}}
        // expected-note@-3 {{force-unwrap the value to avoid this warning}}
        // expected-note@-4 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}
      ] as CFDictionary
    }
  }
}
