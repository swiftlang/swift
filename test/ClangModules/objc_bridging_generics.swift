// RUN: %target-swift-frontend -sdk %S/../Inputs/objc-generics-sdk -I %S/../Inputs/objc-generics-sdk/swift-modules -enable-source-import -parse -parse-as-library -verify %s

// REQUIRES: objc_generics

import Foundation

func testNSArrayBridging(hive: Hive) {
  let bees: [Bee] = hive.bees
}

func testNSDictionaryBridging(hive: Hive) {
  let beesByName: [String : Bee] = hive.beesByName // expected-error{{value of optional type '[String : Bee]?' not unwrapped;}}

  var dict1 = hive.anythingToBees
  var dict2: [NSObject : Bee] = dict1
  dict1 = dict2
}

func testNSSetBridging(hive: Hive) {
  let relatedHives: Set<Bee>= hive.allBees
}

public func expectType<T>(_: T.Type, inout _ x: T) {}

func testNSMutableDictionarySubscript(
  dict: NSMutableDictionary, key: NSCopying, value: AnyObject) {
  var oldValue = dict[key]
  expectType(ImplicitlyUnwrappedOptional<AnyObject>.self, &oldValue)

  dict[key] = value
}
