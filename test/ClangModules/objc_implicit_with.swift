// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

import AppKit

class X {
  init(withInt: Int) { }
  init(withFloat float: Float) { }
  init(withDouble d: Double) { }

  func doSomething(withDouble d: Double) { }

  init(_ withString: String) { } // not corrected
  func doSomething(withString: String) { } // not corrected
}


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
  var of4 = NSObjectFactorySub(float: 314159) // expected-error{{incorrect argument label in call (have 'float:', expected 'integer:')}}
  var of5 = NSObjectFactorySub(buildingWidgets: ()) // expected-error{{cannot find an initializer for type 'NSObjectFactorySub' that accepts an argument list of type '(buildingWidgets: ())'}}
}

func testNSErrorFactoryMethod(path: String) throws {
  var s1 = try NSString(contentsOfFile: path)
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

// rdar://problem/18797808
func testFactoryMethodWithKeywordArgument() {
  var prot = NSCoding.self
  var obj = NSXPCInterface(withProtocol: prot) // not "protocol:"
}
