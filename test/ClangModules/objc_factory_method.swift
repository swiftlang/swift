// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -parse -module-cache-path %t/clang-module-cache %s -verify

import AppKit

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
  var of4 = NSObjectFactorySub(float: 314159) // expected-error{{incorrect argument label in call (have 'float:', expected 'integer:')}}
  var of5 = NSObjectFactorySub(buildingWidgets: ()) // expected-error{{extra argument 'buildingWidgets' in call}}
}

func testNSErrorFactoryMethod(path: String) {
  var error: NSError?
  var s1 = NSString(contentsOfFile: path, error: &error)  // expected-error{{could not find an overload for 'init' that accepts the supplied arguments}}
}

func testNonInstanceTypeFactoryMethod(s: String) {
  var of1 = NSObjectFactory(string: s) // expected-error{{extra argument 'string' in call}}
}

func testUseOfFactoryMethod(queen: B) {
  var of1 = Hive.hiveWithQueen(queen) // expected-error{{'hiveWithQueen' is unavailable: use object construction 'Hive(queen:)'}}
}

func testNonsplittableFactoryMethod() {
  var of5 = NSObjectFactory.factoryBuildingWidgets()
}

func testFactoryMethodBlacklist() {
  var x = NCWidgetController.widgetController()
}
