// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify -swift-version 4 -I %S/Inputs/custom-modules %s

// REQUIRES: objc_interop

import Foundation
import objc_generics
import ObjCBridgeNonconforming

func testNSArrayBridging(_ hive: Hive) {
  _ = hive.bees as [Bee]
}

func testNSDictionaryBridging(_ hive: Hive) {
  _ = hive.beesByName as [String : Bee] // expected-error{{value of optional type '[String : Bee]?' must be unwrapped to a value of type '[String : Bee]'}}
  // expected-note@-1 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
  // expected-note@-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}

  var dict1 = hive.anythingToBees
  let dict2: [AnyHashable : Bee] = dict1
  dict1 = dict2
}

func testNSSetBridging(_ hive: Hive) {
  _ = hive.allBees as Set<Bee>
}

public func expectType<T>(_: T.Type, _ x: inout T) {}

func testNSMutableDictionarySubscript(
  _ dict: NSMutableDictionary, key: NSCopying, value: Any) {
  var oldValue = dict[key]
  expectType(Optional<Any>.self, &oldValue)

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
class Rock: NSObject, Pettable {
  required init(fur: Any) {}
  func other() -> Self { return self }
  class func adopt() -> Self { fatalError("") }
  func pet() {}
  func pet(with other: Pettable) {}
  class var needingMostPets: Pettable {
    get { fatalError("") }
    set { }
  }
}
class Porcupine: Animal {
}
class Cat: Animal, Pettable {
  required init(fur: Any) {}
  func other() -> Self { return self }
  class func adopt() -> Self { fatalError("") }
  func pet() {}
  func pet(with other: Pettable) {}
  class var needingMostPets: Pettable {
    get { fatalError("") }
    set { }
  }
}

func testImportedTypeParamRequirements() {
  let _ = PettableContainer<Desk>() // expected-error{{type 'Desk' does not conform to protocol 'Pettable'}}
  let _ = PettableContainer<Rock>()
  let _ = PettableContainer<Porcupine>() // expected-error{{type 'Porcupine' does not conform to protocol 'Pettable'}}
  let _ = PettableContainer<Cat>()
  let _ = AnimalContainer<Desk>() // expected-error{{'AnimalContainer' requires that 'Desk' inherit from 'Animal'}} // TODO: add test for note appearing in Obj-c header.
  let _ = AnimalContainer<Rock>() // expected-error{{'AnimalContainer' requires that 'Rock' inherit from 'Animal'}} // TODO: add test for note appearing in Obj-c header.
  let _ = AnimalContainer<Porcupine>()
  let _ = AnimalContainer<Cat>()
  let _ = PettableAnimalContainer<Desk>() // expected-error{{'PettableAnimalContainer' requires that 'Desk' inherit from 'Animal'}} // TODO: add test for note appearing in Obj-c header.
  let _ = PettableAnimalContainer<Rock>() // expected-error{{'PettableAnimalContainer' requires that 'Rock' inherit from 'Animal'}} // TODO: add test for note appearing in Obj-c header.
  let _ = PettableAnimalContainer<Porcupine>() // expected-error{{type 'Porcupine' does not conform to protocol 'Pettable'}}
  let _ = PettableAnimalContainer<Cat>()
}

extension GenericClass {
  @objc func doesntUseGenericParam() {}
  @objc func doesntUseGenericParam2() -> Self {}
  // Doesn't use 'T', since ObjC class type params are type-erased
  @objc func doesntUseGenericParam3() -> GenericClass<T> {}
  // Doesn't use 'T', since its metadata isn't necessary to pass around instance
  @objc func doesntUseGenericParam4(_ x: T, _ y: T.Type) -> T {
    _ = x
    _ = y
    return x
  }
  // Doesn't use 'T', since its metadata isn't necessary to erase to AnyObject
  // or to existential metatype
  @objc func doesntUseGenericParam5(_ x: T, _ y: T.Type) -> T {
    _ = y as AnyObject.Type
    _ = y as Any.Type
    _ = y as AnyObject
    _ = x as AnyObject
  }

  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamC(_ x: [(T, T)]?) {} // expected-note{{used here}}
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamD(_ x: Int) {
    _ = T.self // expected-note{{used here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamE(_ x: Int) {
    _ = x as? T // expected-note{{used here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamF(_ x: Int) {
    _ = x is T // expected-note{{used here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamG(_ x: T) {
    _ = T.self // expected-note{{used here}}
  }
  @objc func doesntUseGenericParamH(_ x: T) {
    _ = x as Any
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamI(_ y: T.Type) {
    _ = y as Any // expected-note{{used here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamJ() -> [(T, T)]? {} // expected-note{{used here}}

  @objc static func doesntUseGenericParam() {}
  @objc static func doesntUseGenericParam2() -> Self {}
  // Doesn't technically use 'T', since it's type-erased at runtime
  @objc static func doesntUseGenericParam3() -> GenericClass<T> {}

  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  static func usesGenericParamC(_ x: [(T, T)]?) {} // expected-note{{used here}}
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc static func usesGenericParamD(_ x: Int) {
    _ = T.self // expected-note{{used here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc static func usesGenericParamE(_ x: Int) {
    _ = x as? T // expected-note{{used here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc static func usesGenericParamF(_ x: Int) {
    _ = x is T // expected-note{{used here}}
  }

  @objc func checkThatMethodsAreObjC() {
    _ = #selector(GenericClass.doesntUseGenericParam)
    _ = #selector(GenericClass.doesntUseGenericParam2)
    _ = #selector(GenericClass.doesntUseGenericParam3)
    _ = #selector(GenericClass.doesntUseGenericParam4)
    _ = #selector(GenericClass.doesntUseGenericParam5)
  }
}

func swiftFunction<T: Animal>(x: T) {}

extension AnimalContainer {
  @objc func doesntUseGenericParam1(_ x: T, _ y: T.Type) {
    _ = #selector(x.another)
    _ = #selector(y.create)
  }

  @objc func doesntUseGenericParam2(_ x: T, _ y: T.Type) {
    let a = x.another()
    _ = a.another()
    _ = x.another().another()

    _ = type(of: x).create().another()
    _ = type(of: x).init(noise: x).another()
    _ = y.create().another()
    _ = y.init(noise: x).another()
    _ = y.init(noise: x.another()).another()
    x.eat(a)
  }

  @objc func doesntUseGenericParam3(_ x: T, _ y: T.Type) {
    let sup: Animal = x
    sup.eat(x)
    _ = x.buddy
    _ = x[0]
    x[0] = x
  }

  @objc func doesntUseGenericParam4(_ x: T, _ y: T.Type) {
    _ = type(of: x).apexPredator.another()
    type(of: x).apexPredator = x

    _ = y.apexPredator.another()

    y.apexPredator = x
  }

  @objc func doesntUseGenericParam5(y: T) {
    var x = y
    x = y
    _ = x
  }
  @objc func doesntUseGenericParam6(y: T?) {
    var x = y
    x = y
    _ = x
  }

  // Doesn't use 'T', since dynamic casting to an ObjC generic class doesn't
  // check its generic parameters
  @objc func doesntUseGenericParam7() {
    _ = (self as AnyObject) as! GenericClass<T>
    _ = (self as AnyObject) as? GenericClass<T>
    _ = (self as AnyObject) as! AnimalContainer<T>
    _ = (self as AnyObject) as? AnimalContainer<T>
    _ = (self as AnyObject) is AnimalContainer<T>
    _ = (self as AnyObject) is AnimalContainer<T>
  }

  // Dynamic casting to the generic parameter would require its generic params,
  // though
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamZ1() {
    _ = (self as AnyObject) as! T //expected-note{{here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamZ2() {
    _ = (self as AnyObject) as? T //expected-note{{here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamZ3() {
    _ = (self as AnyObject) is T //expected-note{{here}}
  }


  // expected-note@+2{{add '@objc' to allow uses of 'self' within the function body}}{{3-3=@objc }}
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamA(_ x: T) {
    _ = T(noise: x) // expected-note{{used here}}
  }
  // expected-note@+2{{add '@objc' to allow uses of 'self' within the function body}}{{3-3=@objc }}
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamB() {
    _ = T.create() // expected-note{{used here}}
  }
  // expected-note@+2{{add '@objc' to allow uses of 'self' within the function body}}{{3-3=@objc }}
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamC() {
    _ = T.apexPredator // expected-note{{used here}}
  }
  // expected-note@+2{{add '@objc' to allow uses of 'self' within the function body}}{{3-3=@objc }}
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamD(_ x: T) {
    T.apexPredator = x // expected-note{{used here}}
  }

  // rdar://problem/27796375 -- allocating init entry points for ObjC
  // initializers are generated as true Swift generics, so reify type
  // parameters.
  // expected-note@+2{{add '@objc' to allow uses of 'self' within the function body}}{{3-3=@objc }}
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  func usesGenericParamE(_ x: T) {
    _ = GenericClass(thing: x) // expected-note{{used here}}
  }

  @objc func checkThatMethodsAreObjC() {
    _ = #selector(AnimalContainer.doesntUseGenericParam1)
    _ = #selector(AnimalContainer.doesntUseGenericParam2)
    _ = #selector(AnimalContainer.doesntUseGenericParam3)
    _ = #selector(AnimalContainer.doesntUseGenericParam4)
  }

  // rdar://problem/26283886
  @objc func funcWithWrongArgType(x: NSObject) {}

  @objc func crashWithInvalidSubscript(x: NSArray) {
    _ = funcWithWrongArgType(x: x[12])
    // expected-error@-1{{cannot convert value of type 'Any' to expected argument type 'NSObject'}}
  }
}

extension PettableContainer {
  @objc func doesntUseGenericParam(_ x: T, _ y: T.Type) {
    // TODO: rdar://problem/27796375--allocating entry points are emitted as
    // true generics.
    // _ = type(of: x).init(fur: x).other()
    _ = type(of: x).adopt().other()
    // _ = y.init(fur: x).other()
    _ = y.adopt().other()
    x.pet()
    x.pet(with: x)
  }

  // TODO: rdar://problem/27796375--allocating entry points are emitted as
  // true generics.
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamZ1(_ x: T, _ y: T.Type) {
    _ = type(of: x).init(fur: x).other() // expected-note{{used here}}
  }
  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamZ2(_ x: T, _ y: T.Type) {
    _ = y.init(fur: x).other() // expected-note{{used here}}
  }

  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamA(_ x: T) {
    _ = T(fur: x) // expected-note{{used here}}
  }

  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamB(_ x: T) {
    _ = T.adopt() // expected-note{{used here}}
  }

  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamC(_ x: T) {
    _ = T.needingMostPets // expected-note{{used here}}
  }

  // expected-error@+1{{extension of a generic Objective-C class cannot access the class's generic parameters}}
  @objc func usesGenericParamD(_ x: T) {
    T.needingMostPets = x // expected-note{{used here}}
  }

  @objc func checkThatMethodsAreObjC() {
    _ = #selector(PettableContainer.doesntUseGenericParam)
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

// FIXME: Some generic ObjC APIs rely on covariance. We don't handle this well
// in Swift yet, but ensure we don't emit spurious warnings when
// `as!` is used to force types to line up.
func foo(x: GenericClass<NSMutableString>) {
  let x2 = x as! GenericClass<NSString>
  takeGenericClass(x2)
  takeGenericClass(unsafeBitCast(x, to: GenericClass<NSString>.self))
}

// Test type-erased bounds

func getContainerForPanda() -> AnimalContainer<Animal> {
  return Panda.getContainer()
}

func getContainerForFungiblePanda() -> FungibleAnimalContainer<Animal & Fungible> {
  return Panda.getFungibleContainer()
}

// rdar://problem/30832766 - Infinite recursion while checking conformance
// to AnyObject
let third: Third! = Third()

func useThird() {
  _ = third.description
}


func testNonconforming(bnc: ObjCBridgeNonconforming) {
  let _: Int = bnc.foo // expected-error{{cannot convert value of type 'Set<AnyHashable>' to specified type 'Int'}}
}

func testHashableGenerics(
    any: ObjCBridgeGeneric<ElementConcrete>,
    constrained: ObjCBridgeGenericConstrained<ElementConcrete>,
    insufficient: ObjCBridgeGenericInsufficientlyConstrained<ElementConcrete>,
    extra: ObjCBridgeGenericConstrainedExtra<ElementConcrete>,
    existential: ObjCBridgeExistential) {
  let _: Int = any.foo // expected-error{{cannot convert value of type 'Set<AnyHashable>' to specified type 'Int'}}
  let _: Int = constrained.foo // expected-error{{cannot convert value of type 'Set<ElementConcrete>' to specified type 'Int'}}
  let _: Int = insufficient.foo // expected-error{{cannot convert value of type 'Set<AnyHashable>' to specified type 'Int'}}
  let _: Int = extra.foo // expected-error{{cannot convert value of type 'Set<AnyHashable>' to specified type 'Int'}}
  let _: Int = existential.foo // expected-error{{cannot convert value of type 'Set<AnyHashable>' to specified type 'Int'}}
}

func testGenericsWithTypedefBlocks(hba: HasBlockArray) {
  let _: Int = hba.blockArray() // expected-error{{type '[@convention(block) () -> Void]'}}
}
