// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify

// REQUIRES: objc_interop

import AppKit

class X {
  init(withInt: Int) { }
  init(withFloat float: Float) { }
  init(withDouble d: Double) { }

  func doSomething(withDouble d: Double) { }

  init(_ withString: String) { } // not corrected
  func doSomething(_ withString: String) { } // not corrected
}


func testInitWith(_ url: String) {
  _ = NSInterestingDesignated(url: url)
}

func testInstanceTypeFactoryMethod(_ queen: Bee) {
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
  _ = NSObjectFactorySub(float: 314159) // expected-error{{incorrect argument label in call (have 'float:', expected 'integer:')}}
  let a = NSObjectFactorySub(buildingWidgets: ()) // expected-error{{argument labels '(buildingWidgets:)' do not match any available overloads}}
  // expected-note @-1 {{overloads for 'NSObjectFactorySub' exist with these partially matching parameter lists: (integer: Int), (double: Double)}}
  _ = a
}

func testNSErrorFactoryMethod(_ path: String) throws {
  _ = try NSString(contentsOfFile: path)
}

func testNonInstanceTypeFactoryMethod(_ s: String) {
  _ = NSObjectFactory(string: s) // expected-error{{argument labels '(string:)' do not match any available overloads}}
  // expected-note @-1 {{overloads for 'NSObjectFactory' exist with these partially matching parameter lists: (integer: Int), (double: Double), (float: Float)}}
}

func testUseOfFactoryMethod(_ queen: Bee) {
  _ = Hive.hiveWithQueen(queen) // expected-error{{'hiveWithQueen' has been replaced by 'init(queen:)'}} {{11-25=}} {{26-26=queen: }}
}

func testNonsplittableFactoryMethod() {
  _ = NSObjectFactory.factoryBuildingWidgets()
}

// rdar://problem/18797808
func testFactoryMethodWithKeywordArgument() {
  let prot = NSCoding.self
  _ = NSXPCInterface(with: prot) // not "protocol:"
}
