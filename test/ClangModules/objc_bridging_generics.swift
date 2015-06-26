// RUN: %target-swift-frontend -sdk %S/../Inputs/clang-importer-sdk -I %S/../Inputs/clang-importer-sdk/swift-modules -enable-source-import -parse -parse-as-library -verify %s

// REQUIRES: objc_interop

import Foundation

func testNSArrayBridging(hive: Hive) {
  _ = hive.bees as [Bee]
}

func testNSDictionaryBridging(hive: Hive) {
  _ = hive.beesByName as [String : Bee] // expected-error{{'[String : Bee]?' is not convertible to '[String : Bee]'}}

  var dict1 = hive.anythingToBees
  let dict2: [NSObject : Bee] = dict1
  dict1 = dict2
}

func testNSSetBridging(hive: Hive) {
  _ = hive.allBees as Set<Bee>
}

public func expectType<T>(_: T.Type, inout _ x: T) {}

func testNSMutableDictionarySubscript(
  dict: NSMutableDictionary, key: NSCopying, value: AnyObject) {
  var oldValue = dict[key]
  expectType(Optional<AnyObject>.self, &oldValue)

  dict[key] = value
}
