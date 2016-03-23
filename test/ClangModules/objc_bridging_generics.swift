// RUN: %target-swift-frontend -sdk %S/../Inputs/clang-importer-sdk -I %S/../Inputs/clang-importer-sdk/swift-modules -enable-source-import -enable-import-objc-generics -parse -parse-as-library -verify %s

// REQUIRES: objc_interop

import Foundation
import objc_generics

func testNSArrayBridging(hive: Hive) {
  _ = hive.bees as [Bee]
}

func testNSDictionaryBridging(hive: Hive) {
  _ = hive.beesByName as [String : Bee] // expected-error{{value of optional type '[String : Bee]?' not unwrapped; did you mean to use '!' or '?'?}}

  var dict1 = hive.anythingToBees
  let dict2: [NSObject : Bee] = dict1
  dict1 = dict2
}

func testNSSetBridging(hive: Hive) {
  _ = hive.allBees as Set<Bee>
}

public func expectType<T>(_: T.Type, _ x: inout T) {}

func testNSMutableDictionarySubscript(
  dict: NSMutableDictionary, key: NSCopying, value: AnyObject) {
  var oldValue = dict[key]
  expectType(Optional<AnyObject>.self, &oldValue)

  dict[key] = value
}

class C {}
struct S {}

func f(x: GenericClass<NSString>) -> NSString? { return x.thing() }
func f1(x: GenericClass<NSString>) -> NSString? { return x.otherThing() }
func f2(x: GenericClass<NSString>) -> Int32 { return x.count() }
func f3(x: GenericClass<NSString>) -> NSString? { return x.propertyThing }
func f4(x: GenericClass<NSString>) -> [NSString] { return x.arrayOfThings() }
func f5(x: GenericClass<C>) -> [C] { return x.arrayOfThings() }
func f6(x: GenericSubclass<NSString>) -> NSString? { return x.thing() }
func f6(x: GenericSubclass<C>) -> C? { return x.thing() }

func g() -> NSString? { return GenericClass<NSString>.classThing() }
func g1() -> NSString? { return GenericClass<NSString>.otherClassThing() }

func h(s: NSString?) -> GenericClass<NSString> {
  return GenericClass(thing: s)
}

func j(x: GenericClass<NSString>?) {
  takeGenericClass(x)
}

class Desk {}
class Rock: Pettable {}
class Porcupine: Animal {}
class Cat: Animal, Pettable {}

func testImportedTypeParamRequirements() {
  let _ = PettableContainer<Desk>() // expected-error{{type 'Desk' does not conform to protocol 'Pettable'}}
  let _ = PettableContainer<Rock>()
  let _ = PettableContainer<Porcupine>() // expected-error{{type 'Porcupine' does not conform to protocol 'Pettable'}}
  let _ = PettableContainer<Cat>()
  let _ = AnimalContainer<Desk>() // expected-error{{'AnimalContainer' requires that 'Desk' inherit from 'Animal'}} expected-note{{requirement specified as 'T' : 'Animal' [with T = Desk]}}
  let _ = AnimalContainer<Rock>() // expected-error{{'AnimalContainer' requires that 'Rock' inherit from 'Animal'}} expected-note{{requirement specified as 'T' : 'Animal' [with T = Rock]}}
  let _ = AnimalContainer<Porcupine>()
  let _ = AnimalContainer<Cat>()
  let _ = PettableAnimalContainer<Desk>() // expected-error{{'PettableAnimalContainer' requires that 'Desk' inherit from 'Animal'}} expected-note{{requirement specified as 'T' : 'Animal' [with T = Desk]}}
  let _ = PettableAnimalContainer<Rock>() // expected-error{{'PettableAnimalContainer' requires that 'Rock' inherit from 'Animal'}} expected-note{{requirement specified as 'T' : 'Animal' [with T = Rock]}}
  let _ = PettableAnimalContainer<Porcupine>() // expected-error{{type 'Porcupine' does not conform to protocol 'Pettable'}}
  let _ = PettableAnimalContainer<Cat>()
}

