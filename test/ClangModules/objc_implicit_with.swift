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
  var _ = NSDocument(URL: url)
}

func testInstanceTypeFactoryMethod(queen: B) {
  var _ = Hive(queen: queen)
  
  var _ = NSObjectFactory() // okay, prefers init method
  var _ = NSObjectFactory(integer: 1)
  var _ = NSObjectFactory(double: 314159)
  var _ = NSObjectFactory(float: 314159)
}

func testInstanceTypeFactoryMethodInherited() {
  var _ = NSObjectFactorySub() // okay, prefers init method
  var _ = NSObjectFactorySub(integer: 1)
  var _ = NSObjectFactorySub(double: 314159)
  // FIXME: Awful diagnostic
  var _ = NSObjectFactorySub(float: 314159) // expected-error{{incorrect argument label in call (have 'float:', expected 'integer:')}}
  var _ = NSObjectFactorySub(buildingWidgets: ()) // expected-error{{cannot find an initializer for type 'NSObjectFactorySub' that accepts an argument list of type '(buildingWidgets: ())'}}
}

func testNSErrorFactoryMethod(path: String) throws {
  var _ = try NSString(contentsOfFile: path)
}

func testNonInstanceTypeFactoryMethod(s: String) {
  var _ = NSObjectFactory(string: s) // expected-error{{extra argument 'string' in call}}
}

func testUseOfFactoryMethod(queen: B) {
  var _ = Hive.hiveWithQueen(queen) // expected-error{{'hiveWithQueen' is unavailable: use object construction 'Hive(queen:)'}}
}

func testNonsplittableFactoryMethod() {
  var _ = NSObjectFactory.factoryBuildingWidgets()
}

// rdar://problem/18797808
func testFactoryMethodWithKeywordArgument() {
  let prot = NSCoding.self
  var _ = NSXPCInterface(withProtocol: prot) // not "protocol:"
}
