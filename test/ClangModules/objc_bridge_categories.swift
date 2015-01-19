// RUN: %target-swift-frontend %clang-importer-sdk -parse -I %S/Inputs/custom-modules %s -verify

// Test the use of Objective-C categories on the value types that
// bridge to Objective-C class types.
import Foundation
import AppKit

func testStringBridge(str: String) {
  var str2 = str.nsStringMethod()
  var int = String.nsStringClassMethod()
  var str3 = str.nsStringProperty

  // Make sure the types worked out as expected
  var str: String = str2
  str2 = str
  str = str3
  str3 = str

  var int2: Int = 0
  int = int2

  // Not bridged because it's in the Foundation module.
  str.notBridgedMethod() // expected-error{{'String' does not have a member named 'notBridgedMethod'}}
}

func testDictionaryBridge(dict: Dictionary<String, String>) {
  var d2 = dict.nsDictionaryMethod() // expected-error{{'Dictionary<String, String>' does not have a member named 'nsDictionaryMethod'}}
  var int = Dictionary<String, String>.nsDictionaryClassMethod() // expected-error{{'Dictionary<String, String>.Type' does not have a member named 'nsDictionaryClassMethod'}}
  var d3 = dict.nsDictionaryProperty  // expected-error{{'Dictionary<String, String>' does not have a member named 'nsDictionaryProperty'}}
}

func testStringBridge() {
  var i = String.someFactoryMethod()
  i = 17
}
