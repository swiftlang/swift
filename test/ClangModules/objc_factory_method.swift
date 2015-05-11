// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse %s -verify

// REQUIRES: objc_interop

import AppKit
import NotificationCenter

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

func testFactoryMethodBlacklist() {
  _ = NCWidgetController.widgetController()
  _ = NSProcessInfo.processInfo()
}

func test17261609() {
  let _ = NSDecimalNumber(mantissa:1, exponent:1, isNegative:true)
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
