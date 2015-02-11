// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules -split-objc-selectors %s -verify

// REQUIRES: objc_interop

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
  var i = b.method(1, float:2.5)
  i = i + b.method(1, double:2.5)

  // BOOL
  b.setEnabled(true)

  // SEL
  b.performSelector("isEqual:", object:b) // expected-error {{'performSelector(_:object:)' is unavailable: 'performSelector' methods are unavailable}}

  // Renaming of redundant parameters.
  b.performAdd(1, value:2, value2:3, value:4) // expected-error{{argument 'value' must precede argument 'value2'}}
  b.performAdd(1, value:2, value:4, value2:3)

  // Renaming of redundant parameters.
  b.performMultiply(value:1, value:2)
  
  b.move(`for`: 5)

  // Both class and instance methods exist.
  b.description
  b.instanceTakesObjectClassTakesFloat(b)
  b.instanceTakesObjectClassTakesFloat(2.0) // expected-error{{cannot invoke 'instanceTakesObjectClassTakesFloat' with an argument list of type '(Double)'}}

}

// Class method invocation
func classMethods(b: B, other: NSObject) {
  var i = B.classMethod()
  i += B.classMethod(1)
  i += B.classMethod(1, int:2)

  i += b.classMethod() // expected-error{{'B' does not have a member named 'classMethod'}}
    
  // Both class and instance methods exist.
  let x : AnyObject = B.description()
  B.instanceTakesObjectClassTakesFloat(2.0)
  B.instanceTakesObjectClassTakesFloat(other) // expected-error{{cannot invoke 'instanceTakesObjectClassTakesFloat' with an argument list of type '(NSObject)'}}

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

  let m1 = b.method:_:onCat1:
  m1(1, onCat1: 2.5)

  let m2 = b.method:_:onExtA:
  m2(1, onExtA: 2.5)

  let m3 = b.method:_:onExtB:
  m3(1, onExtB: 2.5)

  let m4 = b.method:_:separateExtMethod:
  m4(1, separateExtMethod: 2.5)
}

func dynamicLookupMethod(b: AnyObject) {
  if let m5 = b.method:_:separateExtMethod: {
    m5(1, separateExtMethod: 2.5)
  }
}

// Properties
func properties(b: B) {
  var i = b.counter
  b.counter = i + 1
  i = i + b.readCounter
  b.readCounter = i + 1 // expected-error{{cannot assign to 'readCounter' in 'b'}}

  b.setCounter(5) // expected-error{{'B' does not have a member named 'setCounter'}}

  // Informal properties in Objective-C map to methods, not variables.
  b.informalProp()

  // An informal property cannot be made formal in a subclass. The
  // formal property is simply ignored.
  b.informalMadeFormal()
  b.informalMadeFormal = i // expected-error{{cannot assign to 'informalMadeFormal' in 'b'}}
  b.setInformalMadeFormal(5)

  b.overriddenProp = 17

  // Dynamic properties.
  var obj : AnyObject = b
  var optStr = obj.nsstringProperty
  if optStr != nil {
    var s : String = optStr!
  }
}

// Construction.
func newConstruction(a: A, aproxy: AProxy) {
  var b : B = B()
  b = B(int: 17)
  b = B(int:17)
  b = B(double:17.5, 3.14159)
  b = B(BBB:b)
  b = B(worldDomination:()) // expected-error{{incorrect argument label in call (have 'worldDomination:', expected 'forWorldDomination:')}}
  b = B(int: 17, andDouble : 3.14159)

  b = B.new(a: a)
  B.alloc()._initFoo()
  b.notAnInit()

  // init methods are not imported by themselves.
  b.initWithInt(17) // expected-error{{'B' does not have a member named 'initWithInt'}}

  // init methods on non-NSObject-rooted classes
  AProxy(int: 5)
}

// Indexed subscripting
func indexedSubscripting(b: B, idx: Int, a: A) {
  b[idx] = a
  var a2 = b[idx] as! A
}

// Keyed subscripting
func keyedSubscripting(b: B, idx: A, a: A) {
  b[a] = a
  var a2 = b[a] as! A
}

// Typed indexed subscripting
func checkHive(hive: Hive, b: B) {
  var b2 = hive.bees[5] as! B
  b2.method(1, float:1.5)
}

// Protocols
func testProtocols(b: B, bp: BProto) {
  var bp2 : BProto = b
  var b2 : B = bp // expected-error{{'BProto' is not convertible to 'B'}}
  bp.method(1, float:2.5)
  bp.method(1, double:2.5) // expected-error{{incorrect argument label in call (have '_:double:', expected '_:float:')}}
  bp2 = b.getAsProto()

  var c1 : Cat1Proto = b
  var bcat1 = b.getAsProtoWithCat()
  c1 = bcat1
  bcat1 = c1 // expected-error{{cannot assign a value of type 'Cat1Proto' to a value of type 'protocol<BProto, Cat1Proto>!'}}
}

// Methods only defined in a protocol
func testProtocolMethods(b: B, p2m: P2.Type) {
  b.otherMethod(1, float:3.14159)
  b.p2Method()
  b.initViaP2(3.14159, second:3.14159) // expected-error{{'B' does not have a member named 'initViaP2'}}

  // Imported constructor.
  var b2 = B(viaP2: 3.14159, second:3.14159)

  // Constructor in protocol.
  p2m(viaP2:3.14159, second: 3.14159) 
}

class MySubclass : B {
  // Override a regular method.
  override func anotherMethodOnB() {}

  // Override a category method
  override func anotherCategoryMethod() {}
}

func getDescription(array: NSArray) {
  array.description
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
  bc[key] = value // expected-error{{cannot assign to the result of this expression}}
}

func almostSubscriptableKeyMismatchInherited(roc: ReadOnlyCollectionChild,
                                             key: String) {
  var value : AnyObject = roc[key] // no-warning, inherited from parent
  roc[key] = value // expected-error{{cannot assign to the result of this expression}}
}

// Use of 'Class' via dynamic lookup.
func classAnyObject(obj: NSObject) {
  obj.myClass().description()
}

// Protocol conformances
class Wobbler : NSWobbling {
  @objc func wobble() { }
  @objc func returnMyself() -> Self { return self }
}

func optionalMemberAccess(w: NSWobbling) {
  w.wobble()
  w.wibble() // expected-error{{value of optional type '(() -> Void)?' not unwrapped; did you mean to use '!' or '?'?}}
  var x: AnyObject = w[5] // expected-error{{value of optional type 'AnyObject!?' not unwrapped; did you mean to use '!' or '?'?}}
}

func protocolInheritance(s: NSString) {
  var coding: NSCoding = s
}

func ivars(hive: Hive) {
  hive.bees.description // no-warning
  hive.queen.description // expected-error{{'Hive' does not have a member named 'queen'}}
}

class NSObjectable : NSObjectProtocol {
  @objc var description : String { return "" }
}


// Properties with custom accessors
func customAccessors(hive: Hive, bee: B) {
  println(hive.makingHoney)
  println(hive.isMakingHoney()) // expected-error{{'Hive' does not have a member named 'isMakingHoney'}}
  hive.setMakingHoney(true) // expected-error{{'Hive' does not have a member named 'setMakingHoney'}}

  hive.guard.description // okay
  hive.guard.description! // no-warning
  hive.guard = bee // no-warning
}

// instancetype/Dynamic Self invocation.
func testDynamicSelf(queen: B, wobbler: NSWobbling) {
  var hive = Hive()

  // Factory method with instancetype result.
  var hive1 = Hive(queen: queen)
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
  w.description
}

class ProtocolAdopter1 : FooProto {
  @objc var bar: CInt // no-warning

  init() { bar = 5 }
}
class ProtocolAdopter2 : FooProto {
  @objc var bar: CInt {
    get { return 42 }
    set { /* do nothing! */ }
  }
}
class ProtocolAdopterBad1 : FooProto { // expected-error{{type 'ProtocolAdopterBad1' does not conform to protocol 'FooProto'}}
  @objc var bar: Int = 0 // expected-note{{candidate has non-matching type 'Int'}}
}
class ProtocolAdopterBad2 : FooProto { // expected-error{{type 'ProtocolAdopterBad2' does not conform to protocol 'FooProto'}}
  let bar: CInt = 0 // expected-note{{candidate is not settable, but protocol requires it}}
}
class ProtocolAdopterBad3 : FooProto { // expected-error{{type 'ProtocolAdopterBad3' does not conform to protocol 'FooProto'}}
  var bar: CInt { // expected-note{{candidate is not settable, but protocol requires it}}
    return 42
  }
}

func splitting(doc: NSDocument, url: NSURL, delegate: MyDelegate) {
  doc.copyDocument(fromURL: url, toURL: url)
  doc.scaleX(by: 5)
  delegate.receiverShouldJumpOnTable(doc)
}

func builtinMapping(view: NSScrollView) {
  view.scrollItemToTop(atIndex: 5)
}

// Multi-word splitting
func multiWordSplit(attrString: NSAttributedString) {
  attrString.sliceAttributedString(5) // note: don't break up "attributed" "string"
}
