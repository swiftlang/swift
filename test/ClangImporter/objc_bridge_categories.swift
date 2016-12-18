// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

// Test the use of Objective-C categories on the value types that
// bridge to Objective-C class types.
import Foundation
import AppKit

func testStringBridge(_ str: String) {
  var str2 = str.nsStringMethod()!
  var int = String.nsStringClassMethod()
  var str3 = str.nsStringProperty!

  // Make sure the types worked out as expected
  var str: String = str2
  str2 = str
  str = str3
  str3 = str

  var int2: Int = 0
  int = int2

  // Not bridged because it's in the Foundation module.
  str.notBridgedMethod() // expected-error{{value of type 'String' has no member 'notBridgedMethod'}}
}

func testDictionaryBridge(_ dict: Dictionary<String, String>) {
  var d2 = dict.nsDictionaryMethod() // expected-error{{value of type 'Dictionary<String, String>' has no member 'nsDictionaryMethod'}}
  var int = Dictionary<String, String>.nsDictionaryClassMethod() // expected-error{{type 'Dictionary<String, String>' has no member 'nsDictionaryClassMethod'}}
  var d3 = dict.nsDictionaryProperty  // expected-error{{value of type 'Dictionary<String, String>' has no member 'nsDictionaryProperty'}}
}

func testStringBridge() {
  var i = String.someFactoryMethod()
  i = 17
  _ = i
}
