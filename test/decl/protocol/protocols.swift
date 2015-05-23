// RUN: %target-parse-verify-swift

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
  static var staticProperty: Int // expected-error{{static stored properties not yet supported in generic types}} expected-error{{property in protocol must have explicit { get } or { get set } specifier}}
}

protocol Test2 {
  var property: Int { get }

  var title: String = "The Art of War" { get } // expected-error{{initial value is not allowed here}} expected-error {{property in protocol must have explicit { get } or { get set } specifier}}
  static var title2: String = "The Art of War" // expected-error{{initial value is not allowed here}} expected-error {{property in protocol must have explicit { get } or { get set } specifier}} expected-error {{static stored properties not yet supported in generic types}}

  typealias mytype
  typealias mybadtype = Int
}

func test1() {
  var v1: Test
  var s: String 
  
  v1.setTitle(s)
  v1.creator = "Me"                   // expected-error {{cannot assign to property: 'creator' is a get-only property}}
}

protocol Bogus : Int {} // expected-error{{inheritance from non-protocol type 'Int'}}

// Explicit conformance checks (successful).
protocol Any { }

protocol CustomStringConvertible { func print() } // expected-note{{protocol requires function 'print()' with type '() -> ()'}} expected-note{{protocol requires}} expected-note{{protocol requires}} expected-note{{protocol requires}}

struct TestFormat { }
protocol FormattedPrintable : CustomStringConvertible {
  func print(format format: TestFormat)
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
  func print(format format: TestFormat) {} // expected-note{{candidate has non-matching type '(format: TestFormat) -> ()'}}
}

// Circular protocols

protocol CircleMiddle : CircleStart { func circle_middle() } // expected-error{{circular protocol inheritance 'CircleMiddle' -> 'CircleStart' -> 'CircleEnd' -> 'CircleMiddle'}}
protocol CircleStart : CircleEnd { func circle_start() } // expected-note{{protocol 'CircleStart' declared here}}
protocol CircleEnd : CircleMiddle { func circle_end()} // expected-note{{protocol 'CircleEnd' declared here}}

protocol CircleEntry : CircleTrivial { }
protocol CircleTrivial : CircleTrivial { } // expected-error{{circular protocol inheritance CircleTrivial}}

struct Circle {
  func circle_start() {}
  func circle_middle() {}
  func circle_end() {}
}

func testCircular(circle: Circle) {
  // FIXME: It would be nice if this failure were suppressed because the protocols
  // have circular definitions.
  _ = circle as CircleStart // expected-error{{'Circle' is not convertible to 'CircleStart'; did you mean to use 'as!' to force downcast?}}
}

// <rdar://problem/14750346>
protocol Q : C, H { }
protocol C : E { }
protocol H : E { }
protocol E { }

struct disallownonglobal { protocol P {} } // expected-error {{declaration is only valid at file scope}}

protocol disallownestp { protocol P {} } // expected-error {{type not allowed here}}
protocol disallownests { struct S {} } // expected-error {{type not allowed here}}
protocol disallownestc { class C {} } // expected-error {{type not allowed here}}
protocol disallownesto { enum O {} } // expected-error {{type not allowed here}}

//===----------------------------------------------------------------------===//
// Associated types
//===----------------------------------------------------------------------===//

protocol SimpleAssoc {
  typealias Associated // expected-note{{protocol requires nested type 'Associated'}}
}

struct IsSimpleAssoc : SimpleAssoc {
  struct Associated {}
}

struct IsNotSimpleAssoc : SimpleAssoc {} // expected-error{{type 'IsNotSimpleAssoc' does not conform to protocol 'SimpleAssoc'}}

protocol StreamWithAssoc {
  typealias Element
  func get() -> Element // expected-note{{protocol requires function 'get()' with type '() -> Element'}}
}

struct AnRange<Int> : StreamWithAssoc {
  typealias Element = Int
  func get() -> Int {}
}

// Okay: Word is a typealias for Int
struct AWordStreamType : StreamWithAssoc {
  typealias Element = Int
  func get() -> Word {}
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
  typealias SequenceStreamTypeType : GeneratorType // expected-note{{protocol requires nested type 'SequenceStreamTypeType'}}
  func generate() -> SequenceStreamTypeType
}

struct IntGeneratorType : GeneratorType /*, SequenceType, ReplPrintable*/ {
  typealias Element = Int
  var min : Int
  var max : Int
  var stride : Int

  mutating func next() -> Int? {
    if min >= max { return .None }
    let prev = min
    min += stride
    return prev
  }

  typealias Generator = IntGeneratorType
  func generate() -> IntGeneratorType {
    return self
  }
}

extension IntGeneratorType : SequenceViaStream {
  typealias SequenceStreamTypeType = IntGeneratorType
}

struct NotSequence : SequenceViaStream { // expected-error{{type 'NotSequence' does not conform to protocol 'SequenceViaStream'}}
  typealias SequenceStreamTypeType = Int // expected-note{{possibly intended match 'SequenceStreamTypeType' does not conform to 'GeneratorType'}}
  func generate() -> Int {}
}

protocol GetATuple {
  typealias Tuple
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
  func increment(value: Int = 1) // expected-error{{default argument not permitted in a protocol method}}
}

struct HasNoDefaultArg : ProtoWithDefaultArg {
  func increment(_: Int) {}
}

//===----------------------------------------------------------------------===//
// Variadic function requirements
//===----------------------------------------------------------------------===//
protocol IntMaxable {
  func intmax(first first: Int, rest: Int...) -> Int // expected-note 2{{protocol requires function 'intmax(first:rest:)' with type '(first: Int, rest: Int...) -> Int'}}
}

struct HasIntMax : IntMaxable {
  func intmax(first first: Int, rest: Int...) -> Int {}
}

struct NotIntMax1 : IntMaxable  { // expected-error{{type 'NotIntMax1' does not conform to protocol 'IntMaxable'}}
  func intmax(first first: Int, rest: [Int]) -> Int {} // expected-note{{candidate has non-matching type '(first: Int, rest: [Int]) -> Int'}}
}

struct NotIntMax2 : IntMaxable { // expected-error{{type 'NotIntMax2' does not conform to protocol 'IntMaxable'}}
  func intmax(first first: Int, rest: Int) -> Int {} // expected-note{{candidate has non-matching type '(first: Int, rest: Int) -> Int'}}
}

//===----------------------------------------------------------------------===//
// 'Self' type
//===----------------------------------------------------------------------===//
protocol IsEqualComparable {
  func isEqual(other other: Self) -> Bool // expected-note{{protocol requires function 'isEqual(other:)' with type '(other: WrongIsEqual) -> Bool'}}
}

struct HasIsEqual : IsEqualComparable {
  func isEqual(other other: HasIsEqual) -> Bool {}
}

struct WrongIsEqual : IsEqualComparable { // expected-error{{type 'WrongIsEqual' does not conform to protocol 'IsEqualComparable'}}
  func isEqual(other other: Int) -> Bool {}  // expected-note{{candidate has non-matching type '(other: Int) -> Bool'}}
}

//===----------------------------------------------------------------------===//
// Using values of existential type.
//===----------------------------------------------------------------------===//

func existentialSequence(e: SequenceType) { // expected-error{{has Self or associated type requirements}}
  // FIXME: Need a more specific diagnostic here.
  var x = e.generate() // expected-error{{'SequenceType' does not have a member named 'generate'}}
  x.next()
  x.nonexistent()
}

protocol HasSequenceAndStream {
  typealias R : GeneratorType, SequenceType
  func getR() -> R
}

func existentialSequenceAndStreamType(h: HasSequenceAndStream) { // expected-error{{has Self or associated type requirements}}
  // FIXME: Crummy diagnostics.
  var x = h.getR() // expected-error{{'HasSequenceAndStream' does not have a member named 'getR'}}
  x.generate()
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
  typealias Element
  subscript (i: Int) -> Element { get }
}

struct DictionaryIntInt {
  subscript (i: Int) -> Int {
    get {
      return i
    }
  }
}

func testSubscripting(iis: IntIntSubscriptable, i_s: IntSubscriptable) { // expected-error{{has Self or associated type requirements}}
  var i: Int = iis[17] 
  // FIXME: Crummy diagnostics.
  var i2 = i_s[17] // expected-error{{'IntSubscriptable' does not have a member named 'subscript'}}
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
  a.f() // expected-error{{'StaticP' does not have a member named 'f'}}
}
func StaticProtocolGenericFunc<t : StaticP>(_: t) {
  t.f()
}

//===----------------------------------------------------------------------===//
// Operators
//===----------------------------------------------------------------------===//
protocol Eq {
  func ==(lhs: Self, rhs: Self) -> Bool
}

extension Int : Eq { }

// Matching prefix/postfix.
prefix operator <> {}
postfix operator <> {}

protocol IndexValue {
  prefix func <> (max: Self) -> Int
  postfix func <> (min: Self) -> Int
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
  typealias Assoc // expected-note 2{{protocol requires nested type 'Assoc'}}
}

protocol P2 {
}

struct X3<T : P1 where T.Assoc : P2> { }

struct X4 : P1 { // expected-error{{type 'X4' does not conform to protocol 'P1'}}
  func getX1() -> X3<X4> { return X3() }
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
    weak var delegate : SecondProtocol? { get } // expected-error{{'weak' cannot be applied to non-class type 'SecondProtocol'}}
}

protocol SecondProtocol {
    func aMethod(object : FirstProtocol)
}

// <rdar://problem/19495341> Can't upcast to parent types of type constraints without forcing
class C1 : P2 {}
func f<T : C1>(x : T) {
  x as P2
}

class C2 {}
func g<T : C2>(x : T) {
  x as P2 // expected-error{{'T' is not convertible to 'P2'; did you mean to use 'as!' to force downcast?}}
}

class C3 : P1 {} // expected-error{{type 'C3' does not conform to protocol 'P1'}}
func h<T : C3>(x : T) {
  x as P1 // expected-error{{protocol 'P1' can only be used as a generic constraint because it has Self or associated type requirements}}
}
