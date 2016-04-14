// RUN: %target-swift-frontend -sdk %S/../Inputs/clang-importer-sdk -I %S/../Inputs/clang-importer-sdk/swift-modules -enable-source-import -enable-import-objc-generics -parse -parse-as-library -verify %s

// REQUIRES: objc_interop

import Foundation
import objc_generics

func testNSArrayBridging(_ hive: Hive) {
  _ = hive.bees as [Bee]
}

func testNSDictionaryBridging(_ hive: Hive) {
  _ = hive.beesByName as [String : Bee] // expected-error{{value of optional type '[String : Bee]?' not unwrapped; did you mean to use '!' or '?'?}}

  var dict1 = hive.anythingToBees
  let dict2: [NSObject : Bee] = dict1
  dict1 = dict2
}

func testNSSetBridging(_ hive: Hive) {
  _ = hive.allBees as Set<Bee>
}

public func expectType<T>(_: T.Type, _ x: inout T) {}

func testNSMutableDictionarySubscript(
  _ dict: NSMutableDictionary, key: NSCopying, value: AnyObject) {
  var oldValue = dict[key]
  expectType(Optional<AnyObject>.self, &oldValue)

  dict[key] = value
}

class C {}
struct S {}

func f(_ x: GenericClass<NSString>) -> NSString? { return x.thing() }
func f1(_ x: GenericClass<NSString>) -> NSString? { return x.otherThing() }
func f2(_ x: GenericClass<NSString>) -> Int32 { return x.count() }
func f3(_ x: GenericClass<NSString>) -> NSString? { return x.propertyThing }
func f4(_ x: GenericClass<NSString>) -> [NSString] { return x.arrayOfThings() }
func f5(_ x: GenericClass<C>) -> [C] { return x.arrayOfThings() }
func f6(_ x: GenericSubclass<NSString>) -> NSString? { return x.thing() }
func f6(_ x: GenericSubclass<C>) -> C? { return x.thing() }

func g() -> NSString? { return GenericClass<NSString>.classThing() }
func g1() -> NSString? { return GenericClass<NSString>.otherClassThing() }

func h(_ s: NSString?) -> GenericClass<NSString> {
  return GenericClass(thing: s)
}

func j(_ x: GenericClass<NSString>?) {
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

extension GenericClass {
  func doesntUseGenericParam() {}
  @objc func doesntUseGenericParam2() -> Self {}
  // Doesn't technically use 'T', since it's type-erased at runtime
  func doesntUseGenericParam3() -> GenericClass<T> {}

  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamA(_ x: T) {}
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamB(_ x: Int) -> T {}
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamC(_ x: [(T, T)]?) {}
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamD(_ x: Int) {
    _ = T.self
  }
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamE(_ x: Int) {
    _ = x as? T
  }
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamF(_ x: Int) {
    _ = x is T
  }

  static func doesntUseGenericParam() {}
  static func doesntUseGenericParam2() -> Self {}
  // Doesn't technically use 'T', since it's type-erased at runtime
  static func doesntUseGenericParam3() -> GenericClass<T> {}

  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  static func usesGenericParamA(_ x: T) {}
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  static func usesGenericParamB(_ x: Int) -> T {}
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  static func usesGenericParamC(_ x: [(T, T)]?) {}
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  static func usesGenericParamD(_ x: Int) {
    _ = T.self
  }
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  static func usesGenericParamE(_ x: Int) {
    _ = x as? T
  }
  // expected-error@+1{{Extension of a generic Objective-C class cannot access the class's generic parameters}}
  static func usesGenericParamF(_ x: Int) {
    _ = x is T
  }

  func checkThatMethodsAreObjC() {
    _ = #selector(GenericClass.doesntUseGenericParam)
    _ = #selector(GenericClass.doesntUseGenericParam2)
    _ = #selector(GenericClass.doesntUseGenericParam3)
  }
}

// expected-error@+1{{inheritance from a generic Objective-C class 'GenericClass' must bind type parameters of 'GenericClass' to specific concrete types}}
class SwiftGenericSubclassA<X: AnyObject>: GenericClass<X> {}
// expected-error@+1{{inheritance from a generic Objective-C class 'GenericClass' must bind type parameters of 'GenericClass' to specific concrete types}}
class SwiftGenericSubclassB<X: AnyObject>: GenericClass<GenericClass<X>> {}
// expected-error@+1{{inheritance from a generic Objective-C class 'GenericClass' must bind type parameters of 'GenericClass' to specific concrete types}}
class SwiftGenericSubclassC<X: NSCopying>: GenericClass<X> {}

class SwiftConcreteSubclassA: GenericClass<AnyObject> {
  override init(thing: AnyObject) { }
  override func thing() -> AnyObject? { }
  override func count() -> Int32 { }
  override class func classThing() -> AnyObject? { }
  override func arrayOfThings() -> [AnyObject] {}
}
class SwiftConcreteSubclassB: GenericClass<NSString> {
  override init(thing: NSString) { }
  override func thing() -> NSString? { }
  override func count() -> Int32 { }
  override class func classThing() -> NSString? { }
  override func arrayOfThings() -> [NSString] {}
}
class SwiftConcreteSubclassC<T>: GenericClass<NSString> {
  override init(thing: NSString) { }
  override func thing() -> NSString? { }
  override func count() -> Int32 { }
  override class func classThing() -> NSString? { }
  override func arrayOfThings() -> [NSString] {}
}

