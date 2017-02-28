// RUN: %target-typecheck-verify-swift
protocol EmptyProtocol { }

protocol DefinitionsInProtocols {
  init() {} // expected-error {{protocol initializers may not have bodies}}
  deinit {} // expected-error {{deinitializers may only be declared within a class}}
}

// Protocol decl.
protocol Test {
  func setTitle(_: String)
  func erase() -> Bool

  var creator: String { get }
  var major : Int { get }
  var minor : Int { get }
  var subminor : Int  // expected-error {{property in protocol must have explicit { get } or { get set } specifier}}
  static var staticProperty: Int // expected-error{{property in protocol must have explicit { get } or { get set } specifier}}
}

protocol Test2 {
  var property: Int { get }

  var title: String = "The Art of War" { get } // expected-error{{initial value is not allowed here}} expected-error {{property in protocol must have explicit { get } or { get set } specifier}}
  static var title2: String = "The Art of War" // expected-error{{initial value is not allowed here}} expected-error {{property in protocol must have explicit { get } or { get set } specifier}}

  associatedtype mytype
  associatedtype mybadtype = Int
}

func test1() {
  var v1: Test
  var s: String 
  
  v1.setTitle(s)
  v1.creator = "Me"                   // expected-error {{cannot assign to property: 'creator' is a get-only property}}
}

protocol Bogus : Int {} // expected-error{{inheritance from non-protocol, non-class type 'Int'}}

// Explicit conformance checks (successful).

protocol CustomStringConvertible { func print() } // expected-note{{protocol requires function 'print()' with type '() -> ()'}} expected-note{{protocol requires}} expected-note{{protocol requires}} expected-note{{protocol requires}}

struct TestFormat { }
protocol FormattedPrintable : CustomStringConvertible {
  func print(format: TestFormat)
}

struct X0 : Any, CustomStringConvertible {
  func print() {}
}

class X1 : Any, CustomStringConvertible {
  func print() {}
}

enum X2 : Any { }

extension X2 : CustomStringConvertible {
  func print() {}
}

// Explicit conformance checks (unsuccessful)

struct NotPrintableS : Any, CustomStringConvertible {} // expected-error{{type 'NotPrintableS' does not conform to protocol 'CustomStringConvertible'}}

class NotPrintableC : CustomStringConvertible, Any {} // expected-error{{type 'NotPrintableC' does not conform to protocol 'CustomStringConvertible'}}

enum NotPrintableO : Any, CustomStringConvertible {} // expected-error{{type 'NotPrintableO' does not conform to protocol 'CustomStringConvertible'}}

struct NotFormattedPrintable : FormattedPrintable { // expected-error{{type 'NotFormattedPrintable' does not conform to protocol 'CustomStringConvertible'}}
  func print(format: TestFormat) {} // expected-note{{candidate has non-matching type '(TestFormat) -> ()'}}
}

// Circular protocols

protocol CircleMiddle : CircleStart { func circle_middle() } // expected-error 2 {{circular protocol inheritance CircleMiddle}}
// expected-error@-1{{circular protocol inheritance 'CircleMiddle' -> 'CircleStart' -> 'CircleEnd' -> 'CircleMiddle'}}
// expected-error @+1 {{circular protocol inheritance CircleStart}}
protocol CircleStart : CircleEnd { func circle_start() } // expected-error 2{{circular protocol inheritance CircleStart}}
// expected-note@-1{{protocol 'CircleStart' declared here}}
protocol CircleEnd : CircleMiddle { func circle_end()} // expected-note{{protocol 'CircleEnd' declared here}}

protocol CircleEntry : CircleTrivial { }
protocol CircleTrivial : CircleTrivial { } // expected-error 3{{circular protocol inheritance CircleTrivial}}

struct Circle {
  func circle_start() {}
  func circle_middle() {}
  func circle_end() {}
}

func testCircular(_ circle: Circle) {
  // FIXME: It would be nice if this failure were suppressed because the protocols
  // have circular definitions.
  _ = circle as CircleStart // expected-error{{'Circle' is not convertible to 'CircleStart'; did you mean to use 'as!' to force downcast?}} {{14-16=as!}}
}

// <rdar://problem/14750346>
protocol Q : C, H { }
protocol C : E { }
protocol H : E { }
protocol E { }

//===----------------------------------------------------------------------===//
// Associated types
//===----------------------------------------------------------------------===//

protocol SimpleAssoc {
  associatedtype Associated // expected-note{{protocol requires nested type 'Associated'}}
}

struct IsSimpleAssoc : SimpleAssoc {
  struct Associated {}
}

struct IsNotSimpleAssoc : SimpleAssoc {} // expected-error{{type 'IsNotSimpleAssoc' does not conform to protocol 'SimpleAssoc'}}

protocol StreamWithAssoc {
  associatedtype Element
  func get() -> Element // expected-note{{protocol requires function 'get()' with type '() -> NotAStreamType.Element'}}
}

struct AnRange<Int> : StreamWithAssoc {
  typealias Element = Int
  func get() -> Int {}
}

// Okay: Word is a typealias for Int
struct AWordStreamType : StreamWithAssoc {
  typealias Element = Int
  func get() -> Int {}
}

struct NotAStreamType : StreamWithAssoc { // expected-error{{type 'NotAStreamType' does not conform to protocol 'StreamWithAssoc'}}
  typealias Element = Float
  func get() -> Int {} // expected-note{{candidate has non-matching type '() -> Int'}}
}

// Okay: Infers Element == Int
struct StreamTypeWithInferredAssociatedTypes : StreamWithAssoc {
  func get() -> Int {}
}

protocol SequenceViaStream {
  associatedtype SequenceStreamTypeType : IteratorProtocol // expected-note{{protocol requires nested type 'SequenceStreamTypeType'}}
  func makeIterator() -> SequenceStreamTypeType
}

struct IntIterator : IteratorProtocol /*, Sequence, ReplPrintable*/ {
  typealias Element = Int
  var min : Int
  var max : Int
  var stride : Int

  mutating func next() -> Int? {
    if min >= max { return .none }
    let prev = min
    min += stride
    return prev
  }

  typealias Generator = IntIterator
  func makeIterator() -> IntIterator {
    return self
  }
}

extension IntIterator : SequenceViaStream {
  typealias SequenceStreamTypeType = IntIterator
}

struct NotSequence : SequenceViaStream { // expected-error{{type 'NotSequence' does not conform to protocol 'SequenceViaStream'}}
  typealias SequenceStreamTypeType = Int // expected-note{{possibly intended match 'NotSequence.SequenceStreamTypeType' (aka 'Int') does not conform to 'IteratorProtocol'}}
  func makeIterator() -> Int {}
}

protocol GetATuple {
  associatedtype Tuple
  func getATuple() -> Tuple
}

struct IntStringGetter : GetATuple {
  typealias Tuple = (i: Int, s: String)
  func getATuple() -> Tuple {}
}

//===----------------------------------------------------------------------===//
// Default arguments
//===----------------------------------------------------------------------===//
// FIXME: Actually make use of default arguments, check substitutions, etc.
protocol ProtoWithDefaultArg {
  func increment(_ value: Int = 1) // expected-error{{default argument not permitted in a protocol method}}
}

struct HasNoDefaultArg : ProtoWithDefaultArg {
  func increment(_: Int) {}
}

//===----------------------------------------------------------------------===//
// Variadic function requirements
//===----------------------------------------------------------------------===//
protocol IntMaxable {
  func intmax(first: Int, rest: Int...) -> Int // expected-note 2{{protocol requires function 'intmax(first:rest:)' with type '(Int, Int...) -> Int'}}
}

struct HasIntMax : IntMaxable {
  func intmax(first: Int, rest: Int...) -> Int {}
}

struct NotIntMax1 : IntMaxable  { // expected-error{{type 'NotIntMax1' does not conform to protocol 'IntMaxable'}}
  func intmax(first: Int, rest: [Int]) -> Int {} // expected-note{{candidate has non-matching type '(Int, [Int]) -> Int'}}
}

struct NotIntMax2 : IntMaxable { // expected-error{{type 'NotIntMax2' does not conform to protocol 'IntMaxable'}}
  func intmax(first: Int, rest: Int) -> Int {} // expected-note{{candidate has non-matching type '(Int, Int) -> Int'}}
}

//===----------------------------------------------------------------------===//
// 'Self' type
//===----------------------------------------------------------------------===//
protocol IsEqualComparable {
  func isEqual(other: Self) -> Bool // expected-note{{protocol requires function 'isEqual(other:)' with type '(WrongIsEqual) -> Bool'}}
}

struct HasIsEqual : IsEqualComparable {
  func isEqual(other: HasIsEqual) -> Bool {}
}

struct WrongIsEqual : IsEqualComparable { // expected-error{{type 'WrongIsEqual' does not conform to protocol 'IsEqualComparable'}}
  func isEqual(other: Int) -> Bool {}  // expected-note{{candidate has non-matching type '(Int) -> Bool'}}
}

//===----------------------------------------------------------------------===//
// Using values of existential type.
//===----------------------------------------------------------------------===//

func existentialSequence(_ e: Sequence) { // expected-error{{has Self or associated type requirements}}
	// FIXME: Weird diagnostic
  var x = e.makeIterator() // expected-error{{'Sequence' is not convertible to 'Sequence.Iterator'}}
  x.next()
  x.nonexistent()
}

protocol HasSequenceAndStream {
  associatedtype R : IteratorProtocol, Sequence
  func getR() -> R
}

func existentialSequenceAndStreamType(_ h: HasSequenceAndStream) { // expected-error{{has Self or associated type requirements}}
  // FIXME: Crummy diagnostics.
  var x = h.getR() // expected-error{{member 'getR' cannot be used on value of protocol type 'HasSequenceAndStream'; use a generic constraint instead}}
  x.makeIterator()
  x.next()

  x.nonexistent()
}

//===----------------------------------------------------------------------===//
// Subscripting
//===----------------------------------------------------------------------===//
protocol IntIntSubscriptable {
  subscript (i: Int) -> Int { get }
}

protocol IntSubscriptable {
  associatedtype Element
  subscript (i: Int) -> Element { get }
}

struct DictionaryIntInt {
  subscript (i: Int) -> Int {
    get {
      return i
    }
  }
}

func testSubscripting(_ iis: IntIntSubscriptable, i_s: IntSubscriptable) { // expected-error{{has Self or associated type requirements}}
  var i: Int = iis[17] 
  var i2 = i_s[17] // expected-error{{member 'subscript' cannot be used on value of protocol type 'IntSubscriptable'; use a generic constraint instead}}
}

//===----------------------------------------------------------------------===//
// Static methods
//===----------------------------------------------------------------------===//
protocol StaticP {
  static func f()
}
protocol InstanceP {
  func f() // expected-note{{protocol requires function 'f()' with type '() -> ()'}}
}
struct StaticS1 : StaticP {
  static func f() {}
}
struct StaticS2 : InstanceP { // expected-error{{type 'StaticS2' does not conform to protocol 'InstanceP'}}
  static func f() {} // expected-note{{candidate operates on a type, not an instance as required}}
}
struct StaticAndInstanceS : InstanceP {
  static func f() {}
  func f() {}
}
func StaticProtocolFunc() {
  let a: StaticP = StaticS1()
  a.f() // expected-error{{static member 'f' cannot be used on instance of type 'StaticP'}}
}
func StaticProtocolGenericFunc<t : StaticP>(_: t) {
  t.f()
}

//===----------------------------------------------------------------------===//
// Operators
//===----------------------------------------------------------------------===//
protocol Eq {
  static func ==(lhs: Self, rhs: Self) -> Bool
}

extension Int : Eq { }

// Matching prefix/postfix.
prefix operator <>
postfix operator <>

protocol IndexValue {
  static prefix func <> (_ max: Self) -> Int
  static postfix func <> (min: Self) -> Int
}

prefix func <> (max: Int) -> Int  { return 0 }
postfix func <> (min: Int) -> Int  { return 0 }

extension Int : IndexValue {}

//===----------------------------------------------------------------------===//
// Class protocols
//===----------------------------------------------------------------------===//

protocol IntrusiveListNode : class {
  var next : Self { get }
}

final class ClassNode : IntrusiveListNode {
  var next : ClassNode = ClassNode()
}

struct StructNode : IntrusiveListNode { // expected-error{{non-class type 'StructNode' cannot conform to class protocol 'IntrusiveListNode'}}
  // expected-error@-1{{value type 'StructNode' cannot have a stored property that references itself}}
  var next : StructNode
}

final class ClassNodeByExtension { }
struct StructNodeByExtension { }

extension ClassNodeByExtension : IntrusiveListNode {
  var next : ClassNodeByExtension {
    get {
      return self
    }
    set {}
  }
}

extension StructNodeByExtension : IntrusiveListNode { // expected-error{{non-class type 'StructNodeByExtension' cannot conform to class protocol 'IntrusiveListNode'}}
  var next : StructNodeByExtension {
    get {
      return self
    }
    set {}
  }
}

final class GenericClassNode<T> : IntrusiveListNode {
  var next : GenericClassNode<T> = GenericClassNode()
}

struct GenericStructNode<T> : IntrusiveListNode { // expected-error{{non-class type 'GenericStructNode<T>' cannot conform to class protocol 'IntrusiveListNode'}}
  // expected-error@-1{{value type 'GenericStructNode<T>' cannot have a stored property that references itself}}
  var next : GenericStructNode<T>
}

// Refined protocols inherit class-ness
protocol IntrusiveDListNode : IntrusiveListNode {
  var prev : Self { get }
}

final class ClassDNode : IntrusiveDListNode {
  var prev : ClassDNode = ClassDNode()
  var next : ClassDNode = ClassDNode()
}

struct StructDNode : IntrusiveDListNode { // expected-error{{non-class type 'StructDNode' cannot conform to class protocol 'IntrusiveDListNode'}}
  // expected-error@-1{{non-class type 'StructDNode' cannot conform to class protocol 'IntrusiveListNode'}}
  // expected-error@-2{{value type 'StructDNode' cannot have a stored property that references itself}}
  var prev : StructDNode
  var next : StructDNode
}

@objc protocol ObjCProtocol {
  func foo() // expected-note{{protocol requires function 'foo()' with type '() -> ()'}}
}
protocol NonObjCProtocol : class { //expected-note{{protocol 'NonObjCProtocol' declared here}}
  func bar()
}

class DoesntConformToObjCProtocol : ObjCProtocol { // expected-error{{type 'DoesntConformToObjCProtocol' does not conform to protocol 'ObjCProtocol'}}
}

@objc protocol ObjCProtocolRefinement : ObjCProtocol { }

@objc protocol ObjCNonObjCProtocolRefinement : NonObjCProtocol { } //expected-error{{@objc protocol 'ObjCNonObjCProtocolRefinement' cannot refine non-@objc protocol 'NonObjCProtocol'}}


// <rdar://problem/16079878>
protocol P1 {
  associatedtype Assoc // expected-note 2{{protocol requires nested type 'Assoc'}}
}

protocol P2 {
}

struct X3<T : P1> where T.Assoc : P2 {}

struct X4 : P1 { // expected-error{{type 'X4' does not conform to protocol 'P1'}}
  func getX1() -> X3<X4> { return X3() } // expected-error{{cannot convert return expression of type 'X3<_>' to return type 'X3<X4>'}}
}

protocol ShouldntCrash {
  // rdar://16109996
  let fullName: String { get }  // expected-error {{'let' declarations cannot be computed properties}}
  
  // <rdar://problem/17200672> Let in protocol causes unclear errors and crashes
  let fullName2: String  // expected-error {{immutable property requirement must be declared as 'var' with a '{ get }' specifier}}

  // <rdar://problem/16789886> Assert on protocol property requirement without a type
  var propertyWithoutType { get } // expected-error {{type annotation missing in pattern}} expected-error {{computed property must have an explicit type}}
}

// rdar://problem/18168866
protocol FirstProtocol {
    weak var delegate : SecondProtocol? { get } // expected-error{{'weak' may only be applied to class and class-bound protocol types, not 'SecondProtocol'}}
}

protocol SecondProtocol {
    func aMethod(_ object : FirstProtocol)
}

// <rdar://problem/19495341> Can't upcast to parent types of type constraints without forcing
class C1 : P2 {}
func f<T : C1>(_ x : T) {
  _ = x as P2
}

class C2 {}
func g<T : C2>(_ x : T) {
  x as P2 // expected-error{{'T' is not convertible to 'P2'; did you mean to use 'as!' to force downcast?}} {{5-7=as!}}
}

class C3 : P1 {} // expected-error{{type 'C3' does not conform to protocol 'P1'}}
func h<T : C3>(_ x : T) {
  _ = x as P1 // expected-error{{protocol 'P1' can only be used as a generic constraint because it has Self or associated type requirements}}
}



protocol P4 {
  associatedtype T // expected-note {{protocol requires nested type 'T'}}
}

class C4 : P4 { // expected-error {{type 'C4' does not conform to protocol 'P4'}}
  associatedtype T = Int  // expected-error {{associated types can only be defined in a protocol; define a type or introduce a 'typealias' to satisfy an associated type requirement}} {{3-17=typealias}}
}
