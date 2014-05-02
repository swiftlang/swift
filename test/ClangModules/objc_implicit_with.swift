// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache -enable-objc-factory-method-constructors -implicit-objc-with %s -verify

import AppKit

func testInitWith(url: String) {
  var doc = NSDocument(URL: url)
}

func testInstanceTypeFactoryMethod(queen: B) {
  var hive1 = Hive(queen: queen)
  
  var of1 = NSObjectFactory() // okay, prefers init method
  var of2 = NSObjectFactory(integer: 1)
  var of3 = NSObjectFactory(double: 314159)
  var of4 = NSObjectFactory(float: 314159)
}

func testInstanceTypeFactoryMethodInherited() {
  var of1 = NSObjectFactorySub() // okay, prefers init method
  var of2 = NSObjectFactorySub(integer: 1)
  var of3 = NSObjectFactorySub(double: 314159)
  // FIXME: Awful diagnostic
  var of4 = NSObjectFactorySub(float: 314159) // expected-error{{tuple types '(float: $T0)' and '()' have a different number of elements (1 vs. 0)}}
  var of5 = NSObjectFactorySub(buildingWidgets: ()) // FIXME: Should fail, but tuple conversions are screwing it up
}

func testNSErrorFactoryMethod(path: String) {
  var error: NSError?
  var s1 = NSString(contentsOfFile: path, error: &error)  // expected-error{{does not type-check}}
}

func testNonInstanceTypeFactoryMethod(s: String) {
  var of1 = NSObjectFactory(string: s) // expected-error{{tuple types '(string: String)' and '()' have a different number of elements (1 vs. 0)}}
}

func testUseOfFactoryMethod(queen: B) {
  var of1 = Hive.hiveWithQueen(queen) // expected-error{{'hiveWithQueen' is unavailable: use object construction 'Hive(queen:)'}}
}

func testNonsplittableFactoryMethod() {
  var of5 = NSObjectFactory.factoryBuildingWidgets()
}
