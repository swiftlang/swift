// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -target x86_64-apple-macosx10.10 -parse %s -verify

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import AppKit
import NotificationCenter

func testInstanceTypeFactoryMethod(queen: Bee) {
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
  _ = NSObjectFactorySub(float: 314159) // expected-error{{incorrect argument label in call (have 'float:', expected 'integer:')}} {{26-31=integer}}
  let a = NSObjectFactorySub(buildingWidgets: ()) // expected-error{{argument labels '(buildingWidgets:)' do not match any available overloads}}
  // expected-note @-1 {{overloads for 'NSObjectFactorySub' exist with these partially matching parameter lists: (integer: Int), (double: Double)}}
  _ = a
}

func testFactoryWithLaterIntroducedInit() {
  // Prefer importing more available factory initializer over less
  // less available convenience initializer
  _ = NSHavingConvenienceFactoryAndLaterConvenienceInit(flim:5)
  _ = NSHavingConvenienceFactoryAndLaterConvenienceInit(flam:5)

  // Prefer importing more available convenience initializer over less
  // less available factory initializer
  _ = NSHavingConvenienceFactoryAndEarlierConvenienceInit(flim:5)
  _ = NSHavingConvenienceFactoryAndEarlierConvenienceInit(flam:5)

  // Don't prefer more available convenience factory initializer over less
  // available designated initializer
  _ = NSHavingConvenienceFactoryAndLaterDesignatedInit(flim:5) // expected-error {{'init(flim:)' is only available on OS X 10.11 or newer}} 
    // expected-note @-1 {{add 'if #available' version check}}
    // expected-note @-2 {{add @available attribute to enclosing global function}}
  
  _ = NSHavingConvenienceFactoryAndLaterDesignatedInit(flam:5) // expected-error {{'init(flam:)' is only available on OS X 10.11 or newer}}
  // expected-note @-1 {{add 'if #available' version check}}  {{3-63=if #available(OSX 10.11, *) {\n      _ = NSHavingConvenienceFactoryAndLaterDesignatedInit(flam:5)\n  \} else {\n      // Fallback on earlier versions\n  \}}}
  // expected-note @-2 {{add @available attribute to enclosing global function}} {{1-1=@available(OSX 10.11, *)\n}}

  
  // Don't prefer more available factory initializer over less
  // available designated initializer
  _ = NSHavingFactoryAndLaterConvenienceInit(flim:5) // expected-error {{'init(flim:)' is only available on OS X 10.11 or newer}} 
  // expected-note @-1 {{add 'if #available' version check}}
  // expected-note @-2 {{add @available attribute to enclosing global function}}

  _ = NSHavingFactoryAndLaterConvenienceInit(flam:5) // expected-error {{'init(flam:)' is only available on OS X 10.11 or newer}} 
  // expected-note @-1 {{add 'if #available' version check}}
  // expected-note @-2 {{add @available attribute to enclosing global function}}


  // When both a convenience factory and a convenience initializer have the
  // same availability, choose the convenience initializer.
  _ = NSHavingConvenienceFactoryAndSameConvenienceInit(flim:5) // expected-warning {{'init(flim:)' was deprecated in OS X 10.10: ConvenienceInit}}
  _ = NSHavingConvenienceFactoryAndSameConvenienceInit(flam:5) // expected-warning {{'init(flam:)' was deprecated in OS X 10.10: ConvenienceInit}}

  _ = NSHavingConvenienceFactoryAndSameConvenienceInit(flotsam:5) // expected-warning {{'init(flotsam:)' is deprecated: ConvenienceInit}}
  _ = NSHavingConvenienceFactoryAndSameConvenienceInit(jetsam:5) // expected-warning {{'init(jetsam:)' is deprecated: ConvenienceInit}}

  _ = NSHavingUnavailableFactoryAndUnavailableConvenienceInit(flim:5) // expected-error {{'init(flim:)' is unavailable: ConvenienceInit}}
  _ = NSHavingUnavailableFactoryAndUnavailableConvenienceInit(flam:5) // expected-error {{'init(flam:)' is unavailable: ConvenienceInit}}
}

func testNSErrorFactoryMethod(path: String) throws {
  _ = try NSString(contentsOfFile: path)
}

func testNonInstanceTypeFactoryMethod(s: String) {
  _ = NSObjectFactory(string: s) // expected-error{{argument labels '(string:)' do not match any available overloads}}
  // expected-note @-1 {{(integer: Int), (double: Double), (float: Float)}}
}

func testUseOfFactoryMethod(queen: Bee) {
  _ = Hive.hiveWithQueen(queen) // expected-error{{'hiveWithQueen' is unavailable: use object construction 'Hive(queen:)'}}
}

func testNonsplittableFactoryMethod() {
  _ = NSObjectFactory.factoryBuildingWidgets()
}

func testFactoryMethodBlacklist() {
  _ = NCWidgetController.widgetController()
  _ = NSProcessInfo.processInfo()
}

func test17261609() {
  _ = NSDecimalNumber(mantissa:1, exponent:1, isNegative:true)
  NSDecimalNumber.decimalNumberWithMantissa(1, exponent:1, isNegative:true) // expected-error{{'decimalNumberWithMantissa(_:exponent:isNegative:)' is unavailable: use object construction 'NSDecimalNumber(mantissa:exponent:isNegative:)'}}
}

func testURL() {
  let url = NSURL(string: "http://www.llvm.org")
  NSURL.URLWithString("http://www.llvm.org") // expected-error{{'URLWithString' is unavailable: use object construction 'NSURL(string:)'}}

  NSURLRequest(string: "http://www.llvm.org") // expected-warning{{unused}}
  NSURLRequest(URL: url) // expected-warning{{unused}}

  NSURLRequest.requestWithString("http://www.llvm.org") // expected-error{{'requestWithString' is unavailable: use object construction 'NSURLRequest(string:)'}}
  NSURLRequest.URLRequestWithURL(url) // expected-error{{'URLRequestWithURL' is unavailable: use object construction 'NSURLRequest(URL:)'}}
}
