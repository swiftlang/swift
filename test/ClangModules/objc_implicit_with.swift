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
  _ = NSInterestingDesignated(URL: url)
}

func testInstanceTypeFactoryMethod(queen: B) {
  _ = Hive(queen: queen)
  
  _ = NSObjectFactory() // okay, prefers init method
  _ = NSObjectFactory(integer: 1)
  _ = NSObjectFactory(double: 314159)
  _ = NSObjectFactory(float: 314159)
}

func testInstanceTypeFactoryMethodInherited() {
  _ = NSObjectFactorySub() // okay, prefers init method
  _ = NSObjectFactorySub(integer: 1)
  _ = NSObjectFactorySub(double: 314159)
  // FIXME: Awful diagnostic
  _ = NSObjectFactorySub(float: 314159) // expected-error{{incorrect argument label in call (have 'float:', expected 'integer:')}}
  let a = NSObjectFactorySub(buildingWidgets: ()) // expected-error{{cannot find an initializer for type 'NSObjectFactorySub' that accepts an argument list of type '(buildingWidgets: ())'}}
  _ = a
}

func testNSErrorFactoryMethod(path: String) throws {
  _ = try NSString(contentsOfFile: path)
}

func testNonInstanceTypeFactoryMethod(s: String) {
  _ = NSObjectFactory(string: s) // expected-error{{extra argument 'string' in call}}
}

func testUseOfFactoryMethod(queen: B) {
  _ = Hive.hiveWithQueen(queen) // expected-error{{'hiveWithQueen' is unavailable: use object construction 'Hive(queen:)'}}
}

func testNonsplittableFactoryMethod() {
  _ = NSObjectFactory.factoryBuildingWidgets()
}

// rdar://problem/18797808
func testFactoryMethodWithKeywordArgument() {
  let prot = NSCoding.self
  _ = NSXPCInterface(withProtocol: prot) // not "protocol:"
}
