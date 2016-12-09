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

  // sanity check
  nsStr = nsMutableStr
}

func testObjCToCF(_ nsStr: NSString, nsMutableStr: NSMutableString) {
  var cfStr: CFString = nsStr
  cfStr = nsMutableStr

  var cfMutableStr: CFMutableString = nsMutableStr
  cfMutableStr = cfStr // expected-error{{cannot assign value of type 'CFString' to type 'CFMutableString'}}

  // sanity check
  cfStr = cfMutableStr
}

func testCFToNative(_ cfStr: CFString, cfMutableStr: CFMutableString) {
  var str = cfStr as String
  str = cfMutableStr as String
  _ = str
}

func testNativeToCF(_ str: String) {
  var cfStr = str as CFString
  var cfMutableStr = str as CFMutableString // expected-error{{'String' is not convertible to 'CFMutableString'}} {{26-28=as!}}
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
  cfStr = anyObject // expected-error{{'AnyObject' is not convertible to 'CFString'; did you mean to use 'as!' to force downcast?}} {{20-20= as! CFString}}
  _ = cfStr
}

func testUncheckableCasts(_ anyObject: AnyObject, nsObject: NSObject,
                          anyObjectType: AnyObject.Type, 
                          nsObjectType: NSObject.Type) {
  if let _ = anyObject as? CFString { } // expected-error{{conditional downcast to CoreFoundation type 'CFString' will always succeed}}
  if let _ = nsObject as? CFString { } // expected-error{{conditional downcast to CoreFoundation type 'CFString' will always succeed}}

  if let _ = anyObject as? CFTree { } // expected-error{{conditional downcast to CoreFoundation type 'CFTree' will always succeed}}
  if let _ = nsObject as? CFTree { } // expected-error{{will always succeed}}

  if let _ = anyObjectType as? CFString.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFString.Type' will always succeed}}
  if let _ = nsObjectType as? CFString.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFString.Type' will always succeed}}

  if let _ = anyObjectType as? CFTree.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFTree.Type' will always succeed}}
  if let _ = nsObjectType as? CFTree.Type { } // expected-error{{will always succeed}}
}

func testCFConvWithIUO(_ x: CFString!, y: NSString!) {
  func acceptCFString(_ a: CFString!) { }
  func acceptNSString(_ b: NSString!) { }

  acceptNSString(x)
  acceptCFString(y)
}
