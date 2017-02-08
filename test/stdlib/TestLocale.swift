// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang %S/Inputs/FoundationBridge/FoundationBridge.m -c -o %t/FoundationBridgeObjC.o -g
// RUN: %target-build-swift %s -I %S/Inputs/FoundationBridge/ -Xlinker %t/FoundationBridgeObjC.o -o %t/TestLocale

// RUN: %target-run %t/TestLocale > %t.txt
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import FoundationBridgeObjC

#if FOUNDATION_XCTEST
    import XCTest
    class TestLocaleSuper : XCTestCase { }
#else
    import StdlibUnittest
    class TestLocaleSuper { }
#endif

class TestLocale : TestLocaleSuper {
    
    func test_bridgingAutoupdating() {
        let tester = LocaleBridgingTester()
        
        do {
            let loc = Locale.autoupdatingCurrent
            let result = tester.verifyAutoupdating(loc)
            expectTrue(result)
        }
        
        do {
            let loc = tester.autoupdatingCurrentLocale()
            let result = tester.verifyAutoupdating(loc)
            expectTrue(result)
        }
    }
    
    func test_equality() {
        let autoupdating = Locale.autoupdatingCurrent
        let autoupdating2 = Locale.autoupdatingCurrent

        expectEqual(autoupdating, autoupdating2)
        
        let current = Locale.current
        
        expectNotEqual(autoupdating, current)
    }
    
    func test_localizedStringFunctions() {
        let locale = Locale(identifier: "en")

        expectEqual("English", locale.localizedString(forIdentifier: "en"))
        expectEqual("France", locale.localizedString(forRegionCode: "fr"))
        expectEqual("Spanish", locale.localizedString(forLanguageCode: "es"))
        expectEqual("Simplified Han", locale.localizedString(forScriptCode: "Hans"))
        expectEqual("Computer", locale.localizedString(forVariantCode: "POSIX"))
        expectEqual("Buddhist Calendar", locale.localizedString(for: .buddhist))
        expectEqual("US Dollar", locale.localizedString(forCurrencyCode: "USD"))
        expectEqual("Phonebook Sort Order", locale.localizedString(forCollationIdentifier: "phonebook"))
        // Need to find a good test case for collator identifier
        // expectEqual("something", locale.localizedString(forCollatorIdentifier: "en"))
    }
    
    func test_properties() {
        let locale = Locale(identifier: "zh-Hant-HK")
        
        expectEqual("zh-Hant-HK", locale.identifier)
        expectEqual("zh", locale.languageCode)
        expectEqual("HK", locale.regionCode)
        expectEqual("Hant", locale.scriptCode)
        expectEqual("POSIX", Locale(identifier: "en_POSIX").variantCode)
        expectTrue(locale.exemplarCharacterSet != nil)
        // The calendar we get back from Locale has the locale set, but not the one we create with Calendar(identifier:). So we configure our comparison calendar first.
        var c = Calendar(identifier: .gregorian)
        c.locale = Locale(identifier: "en_US")
        expectEqual(c, Locale(identifier: "en_US").calendar)
        expectEqual("「", locale.quotationBeginDelimiter)
        expectEqual("」", locale.quotationEndDelimiter)
        expectEqual("『", locale.alternateQuotationBeginDelimiter)
        expectEqual("』", locale.alternateQuotationEndDelimiter)
        expectEqual("phonebook", Locale(identifier: "en_US@collation=phonebook").collationIdentifier)
        expectEqual(".", locale.decimalSeparator)

        
        expectEqual(".", locale.decimalSeparator)
        expectEqual(",", locale.groupingSeparator)
        expectEqual("HK$", locale.currencySymbol)
        expectEqual("HKD", locale.currencyCode)
        
        expectTrue(Locale.availableIdentifiers.count > 0)
        expectTrue(Locale.isoLanguageCodes.count > 0)
        expectTrue(Locale.isoRegionCodes.count > 0)
        expectTrue(Locale.isoCurrencyCodes.count > 0)
        expectTrue(Locale.commonISOCurrencyCodes.count > 0)
        
        expectTrue(Locale.preferredLanguages.count > 0)
        
        // Need to find a good test case for collator identifier
        // expectEqual("something", locale.collatorIdentifier)
    }

    func test_AnyHashableContainingLocale() {
        let values: [Locale] = [
            Locale(identifier: "en"),
            Locale(identifier: "uk"),
            Locale(identifier: "uk"),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Locale.self, type(of: anyHashables[0].base))
        expectEqual(Locale.self, type(of: anyHashables[1].base))
        expectEqual(Locale.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSLocale() {
        let values: [NSLocale] = [
            NSLocale(localeIdentifier: "en"),
            NSLocale(localeIdentifier: "uk"),
            NSLocale(localeIdentifier: "uk"),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Locale.self, type(of: anyHashables[0].base))
        expectEqual(Locale.self, type(of: anyHashables[1].base))
        expectEqual(Locale.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }
}

#if !FOUNDATION_XCTEST
var LocaleTests = TestSuite("TestLocale")
LocaleTests.test("test_bridgingAutoupdating") { TestLocale().test_bridgingAutoupdating() }
LocaleTests.test("test_equality") { TestLocale().test_equality() }
LocaleTests.test("test_localizedStringFunctions") { TestLocale().test_localizedStringFunctions() }
LocaleTests.test("test_properties") { TestLocale().test_properties() }
LocaleTests.test("test_AnyHashableContainingLocale") { TestLocale().test_AnyHashableContainingLocale() }
LocaleTests.test("test_AnyHashableCreatedFromNSLocale") { TestLocale().test_AnyHashableCreatedFromNSLocale() }
runAllTests()
#endif
