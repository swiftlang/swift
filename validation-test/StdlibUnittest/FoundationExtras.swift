// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import StdlibUnittest


import Foundation
import StdlibUnittestFoundationExtras

var FoundationExtrasTests = TestSuite("FoundationExtras")

FoundationExtrasTests.test("withOverriddenLocaleCurrentLocale(Locale)") {
  // Check two locales to make sure the behavior is correct even if one of
  // these locales happens to be the same as the actual current locale.
  do {
    let result = withOverriddenLocaleCurrentLocale(
      Locale(identifier: "en_US") as NSLocale) {
      () -> Int in
      expectEqual("en_US", Locale.current.identifier)
      return 42
    }
    expectEqual(42, result)
  }
  do {
    let result = withOverriddenLocaleCurrentLocale(
      Locale(identifier: "uk") as NSLocale) {
      () -> Int in
      expectEqual("uk", Locale.current.identifier)
      return 42
    }
    expectEqual(42, result)
  }
}

FoundationExtrasTests.test("withOverriddenLocaleCurrentLocale(Locale)/nested") {
  withOverriddenLocaleCurrentLocale(
    Locale(identifier: "uk") as NSLocale) {
    () -> Void in

    expectCrashLater()

    withOverriddenLocaleCurrentLocale(
      Locale(identifier: "uk") as NSLocale) {
      () -> Void in

      return ()
    }
  }
}

FoundationExtrasTests.test("withOverriddenLocaleCurrentLocale(String)") {
  // Check two locales to make sure the behavior is correct even if one of
  // these locales happens to be the same as the actual current locale.
  do {
    let result = withOverriddenLocaleCurrentLocale("en_US") {
      () -> Int in
      expectEqual("en_US", Locale.current.identifier)
      return 42
    }
    expectEqual(42, result)
  }
  do {
    let result = withOverriddenLocaleCurrentLocale("uk") {
      () -> Int in
      expectEqual("uk", Locale.current.identifier)
      return 42
    }
    expectEqual(42, result)
  }
}

@_silgen_name("objc_autorelease")
func objc_autorelease(_ ref: __owned AnyObject)

FoundationExtrasTests.test("objc_autorelease()") {
  autoreleasepool {
    // Check that objc_autorelease indeed autoreleases.
    objc_autorelease(LifetimeTracked(101))
    expectEqual(1, LifetimeTracked.instances)
  }
}

FoundationExtrasTests.test("autoreleasepoolIfUnoptimizedReturnAutoreleased()/autorelease") {
  autoreleasepool {
    autoreleasepoolIfUnoptimizedReturnAutoreleased {
      objc_autorelease(LifetimeTracked(103))
      expectEqual(1, LifetimeTracked.instances)
    }
  }
}

FoundationExtrasTests.test("autoreleasepoolIfUnoptimizedReturnAutoreleased()/return-autoreleased") {
  autoreleasepool {
    autoreleasepoolIfUnoptimizedReturnAutoreleased {
      let nsa = [ LifetimeTracked(104) ] as NSArray
      expectEqual(1, LifetimeTracked.instances)
      _blackHole(nsa[0])
    }
    expectEqual(0, LifetimeTracked.instances)
  }
}

runAllTests()

