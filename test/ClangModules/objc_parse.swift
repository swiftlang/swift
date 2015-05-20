// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import AppKit
import AVFoundation

import objc_ext
import TestProtocols

import ObjCParseExtras
import ObjCParseExtrasToo

func markUsed<T>(t: T) {}

func testAnyObject(obj: AnyObject) {
  _ = obj.nsstringProperty
}

// Construction
func construction() {
  _ = B()
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
  b.performSelector("isEqual:", withObject:b) // expected-error {{'performSelector(_:withObject:)' is unavailable: 'performSelector' methods are unavailable}}

  // Renaming of redundant parameters.
  b.performAdd(1, withValue:2, withValue2:3, withValue:4) // expected-error{{argument 'withValue' must precede argument 'withValue2'}}
  b.performAdd(1, withValue:2, withValue:4, withValue2: 3)

  b.performAdd(1, 2, 3, 4) // expected-error{{missing argument labels 'withValue:withValue:withValue2:' in call}}

  // Both class and instance methods exist.
  b.description
  b.instanceTakesObjectClassTakesFloat(b)
  b.instanceTakesObjectClassTakesFloat(2.0) // expected-error{{cannot invoke 'instanceTakesObjectClassTakesFloat' with an argument list of type '(Double)'}}

  // Instance methods with keyword components
  var obj = NSObject()
  var prot = NSObjectProtocol.self
  b.`protocol`(prot, hasThing:obj)
  b.doThing(obj, `protocol`: prot)
}

func testNSArrayMethods(a: NSArray, b: B) {
  a.makeObjectsPerformSelector("isEqual") // expected-error {{'performSelector' methods are unavailable}}
  a.makeObjectsPerformSelector("isEqual", withObject:b) // expected-error {{'performSelector' methods are unavailable}}
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

  let m1 = b.method:onCat1:
  m1(1, onCat1: 2.5)

  let m2 = b.method:onExtA:
  m2(1, onExtA: 2.5)

  let m3 = b.method:onExtB:
  m3(1, onExtB: 2.5)

  let m4 = b.method:separateExtMethod:
  m4(1, separateExtMethod: 2.5)
}

func dynamicLookupMethod(b: AnyObject) {
  // FIXME: Syntax will be replaced.
  if let m5 = b.method:separateExtMethod: {
    m5(1, separateExtMethod: 2.5)
  }
}

// Properties
func properties(b: B) {
  var i = b.counter
  b.counter = i + 1
  i = i + b.readCounter
  b.readCounter = i + 1 // expected-error{{cannot assign to a get-only property 'readCounter'}}

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

  // Properties that are Swift keywords
  var prot = b.`protocol`
}

// Construction.
func newConstruction(a: A, aproxy: AProxy) {
  var b : B = B()
  b = B(int: 17)
  b = B(int:17)
  b = B(double:17.5, 3.14159)
  b = B(BBB:b)
  b = B(forWorldDomination:())
  b = B(int: 17, andDouble : 3.14159)
  b = B.newWithA(a)
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
  _ = b[idx] as! A
}

// Keyed subscripting
func keyedSubscripting(b: B, idx: A, a: A) {
  b[a] = a
  var a2 = b[a] as! A

  let dict = NSMutableDictionary()
  dict[NSString()] = a
  let value = dict[NSString()]

  dict[nil] = a // expected-error {{cannot assign a value of type 'A' to a value of type 'AnyObject?'}}
  let q = dict[nil]  // expected-error {{type 'NSCopying' does not conform to protocol 'NilLiteralConvertible'}}
  _ = q
}

// Typed indexed subscripting
func checkHive(hive: Hive, b: B) {
  let b2 = hive.bees[5] as! B
  b2.method(1, withFloat:1.5)
}

// Protocols
func testProtocols(b: B, bp: BProto) {
  var bp2 : BProto = b
  var b2 : B = bp // expected-error{{'BProto' is not convertible to 'B'}}
  bp.method(1, withFloat:2.5)
  bp.method(1, withDouble:2.5) // expected-error{{incorrect argument label in call (have '_:withDouble:', expected '_:withFloat:')}}
  bp2 = b.getAsProto()

  var c1 : Cat1Proto = b
  var bcat1 = b.getAsProtoWithCat()
  c1 = bcat1
  bcat1 = c1 // expected-error{{cannot assign a value of type 'Cat1Proto' to a value of type 'protocol<BProto, Cat1Proto>!'}}
}

// Methods only defined in a protocol
func testProtocolMethods(b: B, p2m: P2.Type) {
  b.otherMethod(1, withFloat:3.14159)
  b.p2Method()
  b.initViaP2(3.14159, second:3.14159) // expected-error{{'B' does not have a member named 'initViaP2'}}

  // Imported constructor.
  var b2 = B(viaP2: 3.14159, second:3.14159)

  p2m(viaP2:3.14159, second: 3.14159)
}

func testId(x: AnyObject) {
  x.performSelector!("foo:", withObject: x) // expected-error{{'performSelector(_:withObject:)' is unavailable: 'performSelector' methods are unavailable}}

  x.performAdd(1, withValue: 2, withValue: 3, withValue2: 4)
  x.performAdd!(1, withValue: 2, withValue: 3, withValue2: 4)
  x.performAdd?(1, withValue: 2, withValue: 3, withValue2: 4)
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
  let rs : RefedSub
  rs.overridden()
}

func almostSubscriptableValueMismatch(as1: AlmostSubscriptable, a: A) {
  // FIXME: Crummy diagnostic.
  as1[a] // expected-error{{'AlmostSubscriptable' does not have a member named 'subscript'}}
}

func almostSubscriptableKeyMismatch(bc: BadCollection, key: NSString) {
  // FIXME: We end up importing this as read-only due to the mismatch between
  // getter/setter element types.
  var _ : AnyObject = bc[key]
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
  obj.myClass().description!()
}

// Protocol conformances
class Wobbler : NSWobbling { // expected-note{{candidate is not '@objc', but protocol requires it}}
  // expected-error@-1{{type 'Wobbler' does not conform to protocol 'NSWobbling'}}
  @objc func wobble() { }
  func returnMyself() -> Self { return self } // expected-note{{candidate is not '@objc', but protocol requires it}}
}

extension Wobbler : NSMaybeInitWobble { // expected-error{{type 'Wobbler' does not conform to protocol 'NSMaybeInitWobble'}}
}

@objc class Wobbler2 : NSWobbling { // expected-note{{Objective-C method 'init' provided by implicit initializer 'init()' does not match the requirement's selector ('initWithWobble:')}}
  func wobble() { }
  func returnMyself() -> Self { return self }
}

extension Wobbler2 : NSMaybeInitWobble { // expected-error{{type 'Wobbler2' does not conform to protocol 'NSMaybeInitWobble'}}
}

func optionalMemberAccess(w: NSWobbling) {
  w.wobble()
  w.wibble() // expected-error{{value of optional type '(() -> Void)?' not unwrapped; did you mean to use '!' or '?'?}}
  var x: AnyObject = w[5] // expected-error{{value of optional type 'AnyObject!?' not unwrapped; did you mean to use '!' or '?'?}}
}

func protocolInheritance(s: NSString) {
  var _: NSCoding = s
}

func ivars(hive: Hive) {
  var d = hive.bees.description
  hive.queen.description() // expected-error{{'Hive' does not have a member named 'queen'}}
}

class NSObjectable : NSObjectProtocol {
  @objc var description : String { return "" }
}


// Properties with custom accessors
func customAccessors(hive: Hive, bee: B) {
  markUsed(hive.makingHoney)
  markUsed(hive.isMakingHoney()) // expected-error{{'Hive' does not have a member named 'isMakingHoney'}}
  hive.setMakingHoney(true) // expected-error{{'Hive' does not have a member named 'setMakingHoney'}}

  hive.`guard`.description // okay
  hive.`guard`.description! // no-warning
  hive.`guard` = bee // no-warning
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

  // Instance method on a base class with instancetype result, called on the
  // class itself.
  // FIXME: This should be accepted.
  // FIXME: The error is lousy, too.
  let baseClass: ObjCParseExtras.Base.Type = ObjCParseExtras.Base.returnMyself() // expected-error{{missing argument for parameter #1 in call}}
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

@objc protocol RefinedFooProtocol : FooProto {}

func testPreferClassMethodToCurriedInstanceMethod(obj: NSObject) {
  // FIXME: We shouldn't need the ": Bool" type annotation here.
  // <rdar://problem/18006008>
  let _: Bool = NSObject.isEqual(obj)
  _ = NSObject.isEqual(obj) as (NSObject!) -> Bool // no-warning
}


func testPropertyAndMethodCollision(obj: PropertyAndMethodCollision,
                                    rev: PropertyAndMethodReverseCollision) {
  obj.object = nil
  obj.object(obj, doSomething:"action")

  rev.object = nil
  rev.object(rev, doSomething:"action")

  var value: AnyObject = obj.protoProp()
  value = obj.protoPropRO()
  _ = value
}

func testSubscriptAndPropertyRedeclaration(obj: SubscriptAndProperty) {
  _ = obj.x
  obj.x = 5
  obj.objectAtIndexedSubscript(5) // expected-error{{'objectAtIndexedSubscript' is unavailable: use subscripting}}
  obj.setX(5) // expected-error{{'SubscriptAndProperty' does not have a member named 'setX'}}

  _ = obj[0]
  obj[1] = obj
  obj.setObject(obj, atIndexedSubscript: 2) // expected-error{{'setObject(_:atIndexedSubscript:)' is unavailable: use subscripting}}
}

func testSubscriptAndPropertyWithProtocols(obj: SubscriptAndPropertyWithProto) {
  _ = obj.x
  obj.x = 5
  obj.setX(5) // expected-error{{'SubscriptAndPropertyWithProto' does not have a member named 'setX'}}

  _ = obj[0]
  obj[1] = obj
  obj.setObject(obj, atIndexedSubscript: 2) // expected-error{{'setObject(_:atIndexedSubscript:)' is unavailable: use subscripting}}
}

func testProtocolMappingSameModule(obj: AVVideoCompositionInstruction, p: AVVideoCompositionInstructionProtocol) {
  markUsed(p.enablePostProcessing)
  markUsed(obj.enablePostProcessing)
  _ = obj.backgroundColor
}

func testProtocolMappingDifferentModules(obj: ObjCParseExtrasToo.ProtoOrClass, p: ObjCParseExtras.ProtoOrClass) {
  markUsed(p.thisIsTheProto)
  markUsed(obj.thisClassHasAnAwfulName)

  let _: ProtoOrClass? // expected-error{{'ProtoOrClass' is ambiguous for type lookup in this context}}

  _ = ObjCParseExtrasToo.ClassInHelper() // expected-error{{'ClassInHelper' cannot be constructed because it has no accessible initializers}}
  _ = ObjCParseExtrasToo.ProtoInHelper()
  _ = ObjCParseExtrasTooHelper.ClassInHelper()
  _ = ObjCParseExtrasTooHelper.ProtoInHelper() // expected-error{{'ProtoInHelper' cannot be constructed because it has no accessible initializers}}
}

func testProtocolClassShadowing(obj: ClassInHelper, p: ProtoInHelper) {
  let _: ObjCParseExtrasToo.ClassInHelper = obj
  let _: ObjCParseExtrasToo.ProtoInHelper = p
}


func testNSExtensionContext(url: NSURL, extensionContext: NSExtensionContext) {
  extensionContext.openURL(url) { success in return }
}

func testDealloc(obj: NSObject) {
  // dealloc is subsumed by deinit.
  // FIXME: Special-case diagnostic in the type checker?
  obj.dealloc() // expected-error{{'NSObject' does not have a member named 'dealloc'}}
}

func testConstantGlobals() {
  markUsed(MAX)
  markUsed(SomeImageName)
  markUsed(SomeNumber.description)

  MAX = 5 // expected-error{{cannot assign to 'let' value 'MAX'}}
  SomeImageName = "abc" // expected-error{{cannot assign to 'let' value 'SomeImageName'}}
  SomeNumber = nil // expected-error{{cannot assign to 'let' value 'SomeNumber'}}
}

func testWeakVariable() {
  let _: AnyObject = globalWeakVar
}

class IncompleteProtocolAdopter : Incomplete, IncompleteOptional { // expected-error {{type 'IncompleteProtocolAdopter' cannot conform to protocol 'Incomplete' because it has requirements that cannot be satisfied}}
  func getObject() -> AnyObject { return self }
}

func testNullarySelectorPieces(obj: AnyObject) {
  obj.foo(1, bar: 2, 3) // no-warning
  obj.foo(1, 2, bar: 3) // expected-error{{'AnyObject' does not have a member named 'foo(_:_:bar:)'}}
}

func testFactoryMethodAvailability() {
  _ = DeprecatedFactoryMethod() // expected-warning{{'init()' is deprecated: use something newer}}
}

func testRepeatedMembers(obj: RepeatedMembers) {
  obj.repeatedMethod()
}

// rdar://problem/19726164
class FooDelegateImpl : NSObject, FooDelegate {
  var _started = false
  var started: Bool {
    @objc(isStarted) get { return _started }
    set { _started = newValue }
  }
}

class ProtoAdopter : NSObject, ExplicitSetterProto, OptionalSetterProto {
  var foo: AnyObject? // no errors about conformance
  var bar: AnyObject? // no errors about conformance
}

func testUnusedResults(ur: UnusedResults) {
  _ = ur.producesResult()
  ur.producesResult() // expected-warning{{result of call to 'producesResult()' is unused}}
}
