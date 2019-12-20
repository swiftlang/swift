// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

// REQUIRES: objc_interop

import Foundation

func testDictionary() {
  // -[NSDictionary init] returns non-nil.
  var dictNonOpt = NSDictionary()
  _ = dictNonOpt! // expected-error {{cannot force unwrap value of non-optional type 'NSDictionary'}}
}

func testString() throws {
  // Optional
  let stringOpt = NSString(path: "blah", encoding: 0)
  _ = stringOpt as NSString // expected-error{{value of optional type 'NSString?' must be unwrapped to a value of type 'NSString'}}
  // expected-note@-1 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
  // expected-note@-2 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}

  // Implicitly unwrapped optional
  let stringIUO = NSString(path: "blah")
  if stringIUO == nil { }
  _ = stringIUO as NSString?
  let _: NSString = NSString(path: "blah")
}

func testHive() {
  let hiveIUO = Hive()
  if hiveIUO == nil { }
  _ = hiveIUO as Hive?
  let _: Hive = Hive()
}
