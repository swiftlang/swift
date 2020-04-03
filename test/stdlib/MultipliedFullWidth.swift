//===--- MultipliedFullWidth.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var tests = TestSuite("MultipliedFullWidth")

func testCase<T: FixedWidthInteger>(
  _ x: T, _ y: T, high: T, low: T.Magnitude, line: UInt = #line
) {
  let result = x.multipliedFullWidth(by: y)
  expectEqual(high, result.high, line: line)
  expectEqual(low, result.low, line: line)
}

func specialValues<T: FixedWidthInteger & SignedInteger>(_ type: T.Type) {
  let umin = T.Magnitude(truncatingIfNeeded: T.min)
  testCase(T.min, .min, high: -(.min >> 1), low: 0)
  testCase(T.min, -1,   high: 0, low: umin)
  testCase(T.min,  0,   high: 0, low: 0)
  testCase(T.min,  1,   high: -1, low: umin)
  testCase(T.min, .max, high: .min >> 1, low: umin)
  
  testCase(T(-1), .min, high: 0, low: umin)
  testCase(T(-1), -1,   high: 0, low: 1)
  testCase(T(-1),  0,   high: 0, low: 0)
  testCase(T(-1),  1,   high: -1, low: .max)
  testCase(T(-1), .max, high: -1, low: umin + 1)
  
  testCase(T(0), .min, high: 0, low: 0)
  testCase(T(0), -1,   high: 0, low: 0)
  testCase(T(0),  0,   high: 0, low: 0)
  testCase(T(0),  1,   high: 0, low: 0)
  testCase(T(0), .max, high: 0, low: 0)
  
  testCase(T(1), .min, high: -1, low: umin)
  testCase(T(1), -1,   high: -1, low: .max)
  testCase(T(1),  0,   high: 0, low: 0)
  testCase(T(1),  1,   high: 0, low: 1)
  testCase(T(1), .max, high: 0, low: .max >> 1)
  
  testCase(T.max, .min, high: .min >> 1, low: umin)
  testCase(T.max, -1,   high: -1, low: umin + 1)
  testCase(T.max,  0,   high: 0, low: 0)
  testCase(T.max,  1,   high: 0, low: .max >> 1)
  testCase(T.max, .max, high: (.max >> 1), low: 1)
}

func specialValues<T: FixedWidthInteger & UnsignedInteger>(_ type: T.Type) {
  testCase(T(0),  0,   high: 0, low: 0)
  testCase(T(0),  1,   high: 0, low: 0)
  testCase(T(0), .max, high: 0, low: 0)

  testCase(T(1),  0,   high: 0, low: 0)
  testCase(T(1),  1,   high: 0, low: 1)
  testCase(T(1), .max, high: 0, low: .max)
  
  testCase(T.max,  0,   high: 0, low: 0)
  testCase(T.max,  1,   high: 0, low: .max)
  testCase(T.max, .max, high: .max-1, low: 1)
}

tests.test("Special Values") {
  specialValues(Int.self)
  specialValues(Int64.self)
  specialValues(Int32.self)
  specialValues(Int16.self)
  specialValues(Int8.self)
  
  specialValues(UInt.self)
  specialValues(UInt64.self)
  specialValues(UInt32.self)
  specialValues(UInt16.self)
  specialValues(UInt8.self)
}

tests.test("Random Values") {
  // Some extra coverage for the 64b integers, since they are the only users
  // of the default implementation (only on 32b systems):
  testCase(Int64(-5837700935641288840), -1537421853862307457, high: 486536212510185592, low: 3055263144559363208)
  testCase(Int64(1275671358463268836), 7781435829978284036, high: 538119614841437377, low: 14789118443021950864)
  testCase(Int64(4911382318934676967), -5753361984332212917, high: -1531812888571062585, low: 1722298197364104621)
  testCase(Int64(6581943526282064299), -8155192887281934825, high: -2909837032245044682, low: 16706127436327993437)
  testCase(Int64(4009108071534959395), 7605188192539249328, high: 1652867370329384990, low: 3839516780320392720)
  testCase(Int64(-1272471934452731280), -7713709059189882656, high: 532098144210826160, low: 4919265615377605120)
  testCase(Int64(-1290602245486355209), -6877877092931971073, high: 481201646472028302, low: 4015257672509033225)
  testCase(Int64(1966873427191941886), -7829903732960672311, high: -834858960925259072, low: 12998587081554941806)
  testCase(Int64(5459471085932887725), 7323207134727813062, high: 2167365549637832126, low: 5826569093894448334)
  testCase(Int64(-5681348775805725880), -6546051581806832250, high: 2016095739825622823, low: 7531931343377498032)
  testCase(Int64(3528630718229643203), 6780383198781339163, high: 1297002242834103876, low: 16845851682892995473)
  testCase(Int64(4386302929483327645), 756139473360675718, high: 179796324698125913, low: 13652654049648998702)
  testCase(Int64(-2864416372170195291), 5089997120359086926, high: -790376395292167927, low: 8341529919881354566)
  testCase(Int64(-252886279681789793), 1113918432442210295, high: -15270699648874904, low: 4582052466224525929)
  testCase(Int64(-7821806154093904666), -678157520322455918, high: 287553003647030877, low: 6476241133902266156)
  testCase(Int64(-7739162216163589826), 3946867172269483361, high: -1655871907247741938, low: 13863106094322986622)
  
  testCase(UInt64(4052776605025255999), 17841868768407320997, high: 3919884617339462744, low: 486827993115916699)
  testCase(UInt64(6242835766066895539), 14960190906716810460, high: 5062899690398282642, low: 14718350117826688468)
  testCase(UInt64(17427627038899386484), 13127734187148388607, high: 12402473540330496943, low: 11581729162526677900)
  testCase(UInt64(14992872905705044844), 12933105414992193421, high: 10511578899141219143, low: 7252341782600986236)
  testCase(UInt64(12642327504267244590), 10708397907431293358, high: 7338914274020844379, low: 8873679764824466756)
  testCase(UInt64(18257718462988034339), 17327659287939371125, high: 17150101049683916791, low: 14387647780301477119)
  testCase(UInt64(5589411463208969260), 14342285504591657788, high: 4345749834640583520, low: 12301233398332628560)
  testCase(UInt64(14319626538136147986), 2140855187369381019, high: 1661878466620705928, low: 2387587391530298086)
  testCase(UInt64(12737453267169023056), 10991462038848276938, high: 7589590526017326520, low: 4333382898129426208)
  testCase(UInt64(13832741301927421318), 7713698894698105596, high: 5784305396386691701, low: 6880744869607156712)
  testCase(UInt64(5299309970076755930), 12147554660789977612, high: 3489702967025088672, low: 8435073470527345208)
  testCase(UInt64(3775627568330760013), 12573993794040378591, high: 2573609598696611841, low: 11258650814777796627)
  testCase(UInt64(10163828939432769169), 11406425048812406036, high: 6284737975620258563, low: 8064735036375816276)
  testCase(UInt64(10553402338235046132), 16330771020588292162, high: 9342851854245941300, low: 7535126182307876584)
  testCase(UInt64(17113612905570890777), 11972779332523394977, high: 11107516322766487141, low: 955396679557657273)
  testCase(UInt64(10933450006210087838), 18204962163032170108, high: 10790145018498752040, low: 692054466472649864)
}

runAllTests()
