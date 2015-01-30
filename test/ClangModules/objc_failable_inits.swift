// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

// REQUIRES: objc_interop

import Foundation

func testDictionary() {
  // -[NSDictionary init] returns non-nil.
  var dictNonOpt = NSDictionary()
  if dictNonOpt == nil { } // expected-error{{binary operator '==' cannot be applied to operands of type 'NSDictionary' and 'nil'}}
}

func testString() {
  var error: NSError?

  // Optional
  var stringOpt = NSString(contentsOfFile: "blah", encoding: 0, error: &error)
  var nsStr: NSString = stringOpt // expected-error{{value of optional type 'NSString?' not unwrapped; did you mean to use '!' or '?'?}}

  // Implicitly unwrapped optional
  var stringIUO = NSString(contentsOfFile: "blah", error: &error)
  if stringIUO == nil { }
  var nsStr2: NSString = stringIUO
}

func testHive() {
  var hiveIUO = Hive()
  if hiveIUO == nil { }
  var hive: Hive = hiveIUO
}
