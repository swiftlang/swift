// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

import Foundation

func testDictionary() {
  // -[NSDictionary init] returns non-nil.
  var dictNonOpt = NSDictionary()
  if dictNonOpt == nil { } // expected-error{{binary operator '==' cannot be applied to operands of type 'NSDictionary' and 'nil'}}
}

func testString() throws {
  // Optional
  let stringOpt = NSString(path: "blah", encoding: 0)
  let _: NSString = stringOpt // expected-error{{value of optional type 'NSString?' not unwrapped; did you mean to use '!' or '?'?}}

  // Implicitly unwrapped optional
  let stringIUO = NSString(path: "blah")
  if stringIUO == nil { }
  var _: NSString = stringIUO
}

func testHive() {
  let hiveIUO = Hive()
  if hiveIUO == nil { }
  var _: Hive = hiveIUO
}
