// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -enable-objc-factory-method-constructors %s -verify

import AppKit

func testInstanceTypeFactoryMethod(queen: B) {
  var hive1 = Hive(withQueen: queen)
  
  // FIXME: Need to suppress the class method when there is a corresponding
  // initializer with the same name.
  var of1 = NSObjectFactory() // FIXME: expected-error{{does not type-check}}
  var of2 = NSObjectFactory(withInteger: 1)
  var of3 = NSObjectFactory(withDouble: 314159)
}

func testNSErrorFactoryMethod(path: String) {
  var error: NSError?
  var s1 = NSString(withContentsOfFile: path, error: &error)  // expected-error{{does not type-check}}
}

func testNonInstanceTypeFactoryMethod(s: String) {
  var of1 = NSObjectFactory(withString: s) // expected-error{{does not type-check}}
}
