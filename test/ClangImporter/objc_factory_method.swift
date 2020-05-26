// RUN: %empty-directory(%t)
// RUN: %build-clang-importer-objc-overlays

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -target x86_64-apple-macosx10.51 -typecheck %s -verify

// REQUIRES: OS=macosx
// REQUIRES: objc_interop

import AppKit
import NotificationCenter

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
  let a = NSObjectFactorySub(buildingWidgets: ()) // expected-error{{argument passed to call that takes no arguments}}
  _ = a
}

func testFactoryWithLaterIntroducedInit() {
    // expected-note @-1 4{{add @available attribute to enclosing global function}}
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
  _ = NSHavingConvenienceFactoryAndLaterDesignatedInit(flim:5) // expected-error {{'init(flim:)' is only available in macOS 10.52 or newer}}
    // expected-note @-1 {{add 'if #available' version check}}
  
  _ = NSHavingConvenienceFactoryAndLaterDesignatedInit(flam:5) // expected-error {{'init(flam:)' is only available in macOS 10.52 or newer}}
  // expected-note @-1 {{add 'if #available' version check}}  {{3-63=if #available(OSX 10.52, *) {\n      _ = NSHavingConvenienceFactoryAndLaterDesignatedInit(flam:5)\n  \} else {\n      // Fallback on earlier versions\n  \}}}

  
  // Don't prefer more available factory initializer over less
  // available designated initializer
  _ = NSHavingFactoryAndLaterConvenienceInit(flim:5) // expected-error {{'init(flim:)' is only available in macOS 10.52 or newer}}
  // expected-note @-1 {{add 'if #available' version check}}
  

  _ = NSHavingFactoryAndLaterConvenienceInit(flam:5) // expected-error {{'init(flam:)' is only available in macOS 10.52 or newer}}
  // expected-note @-1 {{add 'if #available' version check}}


  // When both a convenience factory and a convenience initializer have the
  // same availability, choose the convenience initializer.
  _ = NSHavingConvenienceFactoryAndSameConvenienceInit(flim:5) // expected-warning {{'init(flim:)' was deprecated in macOS 10.51: ConvenienceInit}}
  _ = NSHavingConvenienceFactoryAndSameConvenienceInit(flam:5) // expected-warning {{'init(flam:)' was deprecated in macOS 10.51: ConvenienceInit}}

  _ = NSHavingConvenienceFactoryAndSameConvenienceInit(flotsam:5) // expected-warning {{'init(flotsam:)' is deprecated: ConvenienceInit}}
  _ = NSHavingConvenienceFactoryAndSameConvenienceInit(jetsam:5) // expected-warning {{'init(jetsam:)' is deprecated: ConvenienceInit}}

  _ = NSHavingUnavailableFactoryAndUnavailableConvenienceInit(flim:5) // expected-error {{'init(flim:)' is unavailable: ConvenienceInit}}
  _ = NSHavingUnavailableFactoryAndUnavailableConvenienceInit(flam:5) // expected-error {{'init(flam:)' is unavailable: ConvenienceInit}}
}

func testNSErrorFactoryMethod(_ path: String) throws {
  _ = try NSString(contentsOfFile: path)
}

func testNonInstanceTypeFactoryMethod(_ s: String) {
  _ = NSObjectFactory(string: s) // expected-error{{argument passed to call that takes no arguments}}
}

func testUseOfFactoryMethod(_ queen: Bee) {
  _ = Hive.hiveWithQueen(queen) // expected-error{{'hiveWithQueen' has been replaced by 'init(queen:)'}} {{11-25=}} {{26-26=queen: }}
}

func testNonsplittableFactoryMethod() {
  _ = NSObjectFactory.factoryBuildingWidgets()
}

func testFactoryMethodAPINotes() {
  _ = NCWidgetController.widgetController()
}

func test17261609() {
  _ = NSDecimalNumber(mantissa:1, exponent:1, isNegative:true)
  _ = NSDecimalNumber.decimalNumberWithMantissa(1, exponent:1, isNegative:true) // expected-error{{'decimalNumberWithMantissa(_:exponent:isNegative:)' has been replaced by 'init(mantissa:exponent:isNegative:)'}} {{22-48=}} {{49-49=mantissa: }}
}

func testURL() {
  let url = NSURL(string: "http://www.llvm.org")!
  _ = NSURL.URLWithString("http://www.llvm.org") // expected-error{{'URLWithString' has been replaced by 'init(string:)'}} {{12-26=}} {{27-27=string: }}

  NSURLRequest(string: "http://www.llvm.org") // expected-warning{{unused}}
  NSURLRequest(url: url as URL) // expected-warning{{unused}}

  _ = NSURLRequest.requestWithString("http://www.llvm.org") // expected-error{{'requestWithString' has been replaced by 'init(string:)'}}
  _ = NSURLRequest.URLRequestWithURL(url as URL) // expected-error{{'URLRequestWithURL' has been replaced by 'init(url:)'}}
}
