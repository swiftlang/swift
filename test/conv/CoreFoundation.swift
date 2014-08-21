// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache %s -verify

import CoreFoundation
import Foundation

func testCFToObjC(let cfStr: CFString, cfMutableStr: CFMutableString) {
  var nsStr: NSString = cfStr
  nsStr = cfMutableStr

  var nsMutableStr: NSMutableString = cfMutableStr
  nsMutableStr = cfStr // expected-error{{'NSString' is not a subtype of 'NSMutableString'}}

  // sanity check
  nsStr = nsMutableStr
}

func testObjCToCF(let nsStr: NSString, nsMutableStr: NSMutableString) {
  var cfStr: CFString = nsStr
  cfStr = nsMutableStr

  var cfMutableStr: CFMutableString = nsMutableStr
  cfMutableStr = cfStr // expected-error{{'CFString' is not convertible to 'CFMutableString'}}

  // sanity check
  cfStr = cfMutableStr
}
