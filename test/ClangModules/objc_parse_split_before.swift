// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -emit-sil -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules -target x86_64-apple-darwin13 -split-objc-selectors-before %s -verify
// RUN: ls -lR %t/clang-module-cache | grep ObjectiveC.pcm

import AppKit
import objc_ext
import TestProtocols

func testAnyObject(obj: AnyObject) {
  var optStr = obj.nsstringProperty
}

// Construction
func construction() {
  var b = B()
}

// Subtyping
func treatBAsA(b: B) -> A {
  return b
}

// Instance method invocation
func instanceMethods(b: B) {
  var i = b.method(1, withFloat:2.5)
  i = i + b.method(1, withDouble:2.5)

  // BOOL
  b.setEnabled(true)

  // SEL
  b.performSelector("isEqual:", withObject:b)

  // Renaming of redundant parameters.
  b.performAdd(1, withValue:2, withValue2:3, withValue:4)

  // Renaming of redundant parameters.
  b.performMultiply(withValue:1, value:2)
  
  // Splitting does not split a preposition at the end
  b.moveFor(5)

  // Both class and instance methods exist.
  b.description()
  b.instanceTakesObjectClassTakesFloat(b)
  b.instanceTakesObjectClassTakesFloat(2.0) // expected-error{{does not type-check}}

}

// Class method invocation
func classMethods(b: B, other: NSObject) {
  var i = B.classMethod()
  i += B.classMethod(1)
  i += B.classMethod(1, withInt:2)

  i += b.classMethod() // expected-error{{'B' does not have a member named 'classMethod'}}
    
  // Both class and instance methods exist.
  B.description()
  B.instanceTakesObjectClassTakesFloat(2.0)
  B.instanceTakesObjectClassTakesFloat(other) // expected-error{{does not type-check}}

  // Call an instance method of NSObject.
  var c: AnyClass = B.myClass() // no-warning
  c = b.myClass() // no-warning
}

// Instance method invocation on extensions
func instanceMethodsInExtensions(b: B) {
  b.method(1, onCat1:2.5)
  b.method(1, onExtA:2.5)
  b.method(1, onExtB:2.5)
  b.method(1, separateExtMethod:3.5)

  let m1 = b.method:onCat1: // expected-error{{partial application of Objective-C method is not allowed}}
  m1(1, 2.5)

  let m2 = b.method:onExtA: // expected-error{{partial application of Objective-C method is not allowed}}
  m2(1, 2.5)

  let m3 = b.method:onExtB: // expected-error{{partial application of Objective-C method is not allowed}}
  m3(1, 2.5)

  let m4 = b.method:separateExtMethod: // expected-error{{partial application of Objective-C method is not allowed}}
  m4(1, 2.5)
}

func dynamicLookupMethod(b: AnyObject) {
  if let m5 = b.method:separateExtMethod: {
    m5(1, 2.5)
  }
}

// Properties
func properties(b: B) {
  var i = b.counter
  b.counter = i + 1
  i = i + b.readCounter
  b.readCounter = i + 1 // expected-error{{cannot assign to the result of this expression}}

  b.setCounter(5) // expected-error{{'B' does not have a member named 'setCounter'}}

  // Informal properties in Objective-C map to methods, not variables.
  b.informalProp()

  // An informal property cannot be made formal in a subclass. The
  // formal property is simply ignored.
  b.informalMadeFormal()
  b.informalMadeFormal = i // expected-error{{cannot assign to the result of this expression}}
  b.setInformalMadeFormal(5)

  b.overriddenProp = 17

  // Dynamic properties.
  var obj : AnyObject = b
  var optStr = obj.nsstringProperty
  if optStr {
    var s : String = optStr!
  }
}

// Construction.
func newConstruction(a: A, aproxy: AProxy) {
  var b : B = B()
  b = B(17)
  b = B(withInt:17)
  b = B(withDouble:17.5, 3.14159)
  b = B(BBB:b)
  b = B(forWorldDomination:())
  b = B(17, andDouble : 3.14159)

  b = B.`new`(withA:a)
  B.alloc()._initFoo()
  b.notAnInit()

  // init methods are not imported by themselves.
  b.initWithInt(17) // expected-error{{'B' does not have a member named 'initWithInt'}}

  // init methods on non-NSObject-rooted classes
  AProxy(5)
}

// Indexed subscripting
func indexedSubscripting(b: B, idx: Int, a: A) {
  b[idx] = a
  var a2 = (b[idx] as A)!
}

// Keyed subscripting
func keyedSubscripting(b: B, idx: A, a: A) {
  b[a] = a
  var a2 = (b[a] as A)!
}

// Typed indexed subscripting
func checkHive(hive: Hive, b: B) {
  var b2 = (hive.bees[5] as B)!
  b2.method(1, withFloat:1.5)
}

// Protocols
func testProtocols(b: B, bp: BProto) {
  var bp2 : BProto = b
  var b2 : B = bp // expected-error{{'BProto' is not convertible to 'B'}}
  bp.method(1, withFloat:2.5)
  bp.method(1, withDouble:2.5) // expected-error{{expression does not type-check}}
  bp2 = b.getAsProto()

  var c1 : Cat1Proto = b
  var bcat1 = b.getAsProtoWithCat()
  c1 = bcat1
  bcat1 = c1 // expected-error{{type 'Cat1Proto' does not conform to protocol 'BProto'}}
}

// Methods only defined in a protocol
func testProtocolMethods(b: B) {
  b.otherMethod(1, withFloat:3.14159)
  b.p2Method()
  b.initViaP2(3.14159, second:3.14159)

  // Imported constructor.
  var b2 = B(3.14159, second:3.14159)
}

func testId(x: AnyObject) {
  x.performSelector!("foo:", x)
}

class MySubclass : B {
  // Override a regular method.
  @override func anotherMethodOnB() {}

  // Override a category method
  @override func anotherCategoryMethod() {}
}

func getDescription(array: NSArray) {
  array.description()
}

// Method overriding with unfortunate ordering.
func overridingTest(srs: SuperRefsSub) {
  var rs : RefedSub
  rs.overridden()
}

func almostSubscriptableValueMismatch(as1: AlmostSubscriptable, a: A) {
  // FIXME: Crummy diagnostic.
  as1[a] // expected-error{{'AlmostSubscriptable' does not have a member named 'subscript'}}
}

func almostSubscriptableKeyMismatch(bc: BadCollection, key: NSString) {
  // FIXME: We end up importing this as read-only due to the mismatch between
  // getter/setter element types.
  var v : AnyObject = bc[key]
}

func almostSubscriptableKeyMismatchInherited(bc: BadCollectionChild,
                                             key: String) {
  var value : AnyObject = bc[key] // no-warning, inherited from parent
  bc[key] = value // expected-error{{expression does not type-check}}
}

func almostSubscriptableKeyMismatchInherited(roc: ReadOnlyCollectionChild,
                                             key: String) {
  var value : AnyObject = roc[key] // no-warning, inherited from parent
  roc[key] = value // expected-error{{expression does not type-check}}
}

// Use of 'Class' via dynamic lookup.
func classAnyObject(obj: NSObject) {
  obj.myClass().description!()
}

// Protocol conformances
class Wobbler : NSWobbling {
  func wobble() { }
  func returnMyself() -> Self { return self }
}

func optionalMemberAccess(w: NSWobbling) {
  w.wobble()
  w.wibble() // expected-error{{'() -> $T3' is not identical to '(() -> Void)?'}}
  var x: AnyObject = w[5] // expected-error{{type 'AnyObject?' does not conform to protocol 'AnyObject'}}
}

func protocolInheritance(s: NSString) {
  var coding: NSCoding = s
}

func ivars(hive: Hive) {
  hive.bees.description() // no-warning
  hive.queen.description() // expected-error{{'Hive' does not have a member named 'queen'}}
}

class NSObjectable : NSObjectProtocol {
  @objc func description() -> AnyObject { }
}


// Properties with custom accessors
func customAccessors(hive: Hive, bee: B) {
  println(hive.makingHoney)
  println(hive.isMakingHoney()) // expected-error{{'Hive' does not have a member named 'isMakingHoney'}}
  hive.setMakingHoney(true) // expected-error{{'Hive' does not have a member named 'setMakingHoney'}}

  hive.guard.description() // okay
  hive.guard.description!() // no-warning
  hive.guard = bee // no-warning
}

// instancetype/Dynamic Self invocation.
func testDynamicSelf(queen: B, wobbler: NSWobbling) {
  var hive = Hive()

  // Factory method with instancetype result.
  var hive1 = Hive.hive(withQueen: queen)
  hive1 = hive
  hive = hive1

  // Instance method with instancetype result.
  var hive2 = hive.visit()
  hive2 = hive
  hive = hive2

  // Instance method on a protocol with instancetype result.
  var wobbler2 = wobbler.returnMyself()
  var wobbler: NSWobbling = wobbler2
  wobbler2 = wobbler
}

func testRepeatedProtocolAdoption(w: NSWindow) {
  w.description()
}

class ProtocolAdopter1 : FooProto {
  var bar: CInt // no-warning

  init() { bar = 5 }
}
class ProtocolAdopter2 : FooProto {
  var bar: CInt {
    get { return 42 }
    set { /* do nothing! */ }
  }
}
class ProtocolAdopterBad1 : FooProto { // expected-error{{type 'ProtocolAdopterBad1' does not conform to protocol 'FooProto'}}
  var bar: Int = 0 // expected-note{{candidate has non-matching type 'Int'}}
}
class ProtocolAdopterBad2 : FooProto { // expected-error{{type 'ProtocolAdopterBad2' does not conform to protocol 'FooProto'}}
  let bar: CInt = 0 // expected-note{{candidate is not settable, but protocol requires it}}
}
class ProtocolAdopterBad3 : FooProto { // expected-error{{type 'ProtocolAdopterBad3' does not conform to protocol 'FooProto'}}
  var bar: CInt { // expected-note{{candidate is not settable, but protocol requires it}}
    return 42
  }
}

// Subclassing and designated initializers
func testNSInterestingDesignated() {
  NSInterestingDesignated()
  NSInterestingDesignated(withString:"hello")
  NSInterestingDesignatedSub()
  NSInterestingDesignatedSub(withString:"hello")
}

class MyDocument1 : NSDocument {
  init() { 
    super.init()
  }
}

func createMyDocument1() {
  var md = MyDocument1()
  md = MyDocument1(withURL: "http://llvm.org")
}

class MyDocument2 : NSDocument {
  init withURL(url: String) {
    return super.init(withURL: url) // expected-error{{must call a designated initializer of the superclass 'NSDocument'}}
  }
}

class MyDocument3 : NSAwesomeDocument {
  init() { 
    super.init()
  }
}

func createMyDocument3() {
  var md = MyDocument3()
  md = MyDocument3(withURL: "http://llvm.org")
}

class MyInterestingDesignated : NSInterestingDesignatedSub { 
  init withString(str: String) {
    super.init(withString: str)
  }

  init withInt(i: Int) {
    super.init() // expected-error{{must call a designated initializer of the superclass 'NSInterestingDesignatedSub'}}
  }
}

func createMyInterestingDesignated() {
  var md = MyInterestingDesignated(withURL: "http://llvm.org")
}

func testNoReturn(a : NSAwesomeDocument) -> Int {
  a.noReturnMethod(42)
  return 17    // TODO: In principle, we should produce an unreachable code diagnostic here.
}

