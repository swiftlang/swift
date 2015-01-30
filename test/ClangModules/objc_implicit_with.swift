// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

// REQUIRES: objc_interop

import AppKit

class X {
  init(withInt: Int) { } // expected-error{{'with' is implied for the first parameter of an initializer; did you mean to name this parameter 'int'?}}{{8-8=int }}
  init(withFloat float: Float) { } // expected-error{{'with' is implied for the first parameter of an initializer; did you mean to name this parameter 'float'?}}{{8-18=}}
  init(withDouble d: Double) { } // expected-error{{'with' is implied for the first parameter of an initializer; did you mean to name this parameter 'double'?}}{{8-18=double}}

  func doSomething(#withInt: Int) { } // expected-error{{'with' is implied for the first parameter of a method; did you mean to name this parameter 'int'?}}{{21-21=int }}{{20-21=}}
  func doSomething(withFloat float: Float) { } // expected-error{{'with' is implied for the first parameter of a method; did you mean to name this parameter 'float'?}}{{20-30=}}{{30-30=#}}
  func doSomething(withDouble d: Double) { } // expected-error{{'with' is implied for the first parameter of a method; did you mean to name this parameter 'double'?}}{{20-30=double}}

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

func testNSErrorFactoryMethod(path: String) {
  var error: NSError?
  var s1 = NSString(contentsOfFile: path, error: &error)
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
