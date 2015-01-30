// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

// REQUIRES: objc_interop

import CoreFoundation
import Foundation

func testCFToObjC(cfStr: CFString, cfMutableStr: CFMutableString) {
  var nsStr: NSString = cfStr
  nsStr = cfMutableStr

  var nsMutableStr: NSMutableString = cfMutableStr
  nsMutableStr = cfStr // expected-error{{cannot assign a value of type 'CFString' to a value of type 'NSMutableString'}}

  // sanity check
  nsStr = nsMutableStr
}

func testObjCToCF(nsStr: NSString, nsMutableStr: NSMutableString) {
  var cfStr: CFString = nsStr
  cfStr = nsMutableStr

  var cfMutableStr: CFMutableString = nsMutableStr
  cfMutableStr = cfStr // expected-error{{cannot assign a value of type 'CFString' to a value of type 'CFMutableString'}}

  // sanity check
  cfStr = cfMutableStr
}

func testCFToNative(cfStr: CFString, cfMutableStr: CFMutableString) {
  var str = cfStr as String
  str = cfMutableStr as String
}

func testNativeToCF(str: String) {
  var cfStr = str as CFString
  var cfMutableStr = str as CFMutableString // expected-error{{'String' is not convertible to 'CFMutableString'}}
}

func testCFToAnyObject(cfStr: CFString, cfMutableStr: CFMutableString,
                       cfTree: CFTree) {
  var anyObject: AnyObject = cfStr
  anyObject = cfMutableStr
  anyObject = cfTree
}

func testAnyObjectToCF(anyObject: AnyObject) {
  var cfStr: CFString = anyObject as! CFString
  var cfMutableStr: CFMutableString = anyObject as! CFMutableString
  var cfTree: CFTree = anyObject as! CFTree

  // No implicit conversions.
  cfStr = anyObject // expected-error{{'AnyObject' is not convertible to 'CFString'; did you mean to use 'as!' to force downcast?}}
}

func testUncheckableCasts(anyObject: AnyObject, nsObject: NSObject,
                          anyObjectType: AnyObject.Type, 
                          nsObjectType: NSObject.Type) {
  if let cfStr = anyObject as? CFString { } // expected-error{{conditional downcast to CoreFoundation type 'CFString' will always succeed}}
  if let cfStr = nsObject as? CFString { } // expected-error{{conditional downcast to CoreFoundation type 'CFString' will always succeed}}

  if let cfTree = anyObject as? CFTree { } // expected-error{{conditional downcast to CoreFoundation type 'CFTree' will always succeed}}
  if let cfTree = nsObject as? CFTree { } // expected-error{{will always succeed}}

  if let cfStrType = anyObjectType as? CFString.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFString.Type' will always succeed}}
  if let cfStrType = nsObjectType as? CFString.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFString.Type' will always succeed}}

  if let cfTreeType = anyObjectType as? CFTree.Type { } // expected-error{{conditional downcast to CoreFoundation type 'CFTree.Type' will always succeed}}
  if let cfTreeType = nsObjectType as? CFTree.Type { } // expected-error{{will always succeed}}
}

func testCFConvWithIUO(x: CFString!, y: NSString!) {
  func acceptCFString(a: CFString!) { }
  func acceptNSString(b: NSString!) { }

  acceptNSString(x)
  acceptCFString(y)
}
