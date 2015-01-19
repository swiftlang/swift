// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify

import AppKit
import NotificationCenter

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
  var of5 = NSObjectFactorySub(buildingWidgets: ()) // expected-error{{cannot invoke initializer for type 'NSObjectFactorySub' with an argument list of type '(())'}}
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

func testFactoryMethodBlacklist() {
  _ = NCWidgetController.widgetController()
  _ = NSProcessInfo.processInfo()
}

func test17261609() {
  let num1 = NSDecimalNumber(mantissa:1, exponent:1, isNegative:true)
  NSDecimalNumber.decimalNumberWithMantissa(1, exponent:1, isNegative:true) // expected-error{{'decimalNumberWithMantissa(_:exponent:isNegative:)' is unavailable: use object construction 'NSDecimalNumber(mantissa:exponent:isNegative:)'}}
}

func testURL() {
  let url = NSURL(string: "http://www.llvm.org")
  NSURL.URLWithString("http://www.llvm.org") // expected-error{{'URLWithString' is unavailable: use object construction 'NSURL(string:)'}}

  NSURLRequest(string: "http://www.llvm.org")
  NSURLRequest(URL: url)

  NSURLRequest.requestWithString("http://www.llvm.org") // expected-error{{'requestWithString' is unavailable: use object construction 'NSURLRequest(string:)'}}
  NSURLRequest.URLRequestWithURL(url) // expected-error{{'URLRequestWithURL' is unavailable: use object construction 'NSURLRequest(URL:)'}}
}
