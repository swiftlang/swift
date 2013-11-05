// RUN: %swift -parse %s -verify

protocol EmptyProtocol { }

protocol DefinitionsInProtocols {
  init() {} // expected-error {{initializers may only be declared within a class, struct, or enum}}
  destructor() {} // expected-error {{'destructor' functions may only be declared within a class}}
  def fooFunc() {} // expected-error {{function body not allowed here}}
  static def fooStaticFunc() {} // expected-error {{function body not allowed here}}
}

protocol ConstructorInProtocol {
  init() // expected-error {{initializers may only be declared within a class, struct, or enum}} expected-error {{expected '{' for initializer}}
}

// Protocol decl.
protocol Test {
  def setTitle(_: String)
  def erase() -> Bool

  var creator: String
  var (major, minor, subminor): (Int, Int, Int)
}

protocol Test2 {
  def whatsit() -> Bool { // expected-error{{function body not allowed here}}
    return false
  }

  var property: Int { // expected-error{{'var' declarations with getter/setter not allowed here}}
    get:
  }  

  var title: String = "The Art of War" // expected-error{{initial value is not allowed here}}

  typealias mytype
  typealias mybadtype = Int // expected-error{{typealias 'mybadtype' in protocol cannot have a definition}}
}

def test1() {
  var v1: Test
  var s: String
  
  v1.setTitle(s)
  v1.creator = "Me"
}

protocol Bogus : Int {} // expected-error{{inheritance from non-protocol type 'Int'}}

// Explicit conformance checks (successful).
protocol Any { }

protocol Printable { def print() } // expected-note{{protocol requires function 'print' with type '() -> ()'}} expected-note{{protocol requires}} expected-note{{protocol requires}} expected-note{{protocol requires}}

struct TestFormat { }
protocol FormattedPrintable : Printable { // expected-note{{type 'NotFormattedPrintable' does not conform to inherited protocol 'Printable'}}
  def print(format: TestFormat) // expected-note{{protocol requires function 'print' with type '(format: TestFormat) -> ()'}}
}

struct X0 : Any, Printable {
  def print() {}
}

class X1 : Any, Printable {
  def print() {}
}

enum X2 : Any { }

extension X2 : Printable {
  def print() {}
}

// Explicit conformance checks (unsuccessful)

struct NotPrintableS : Any, Printable {} // expected-error{{type 'NotPrintableS' does not conform to protocol 'Printable'}}

class NotPrintableC : Printable, Any {} // expected-error{{type 'NotPrintableC' does not conform to protocol 'Printable'}}

enum NotPrintableO : Any, Printable {} // expected-error{{type 'NotPrintableO' does not conform to protocol 'Printable'}}

struct NotFormattedPrintable : FormattedPrintable { // expected-error{{type 'NotFormattedPrintable' does not conform to protocol 'Printable'}}
  def print(format: TestFormat) {} // expected-note{{candidate has non-matching type '(format: TestFormat) -> ()'}}
}

// Circular protocols

protocol CircleMiddle : CircleStart { def circle_middle() } // expected-error{{circular protocol inheritance 'CircleMiddle' -> 'CircleStart' -> 'CircleEnd' -> 'CircleMiddle'}}
protocol CircleStart : CircleEnd { def circle_start() } // expected-note{{protocol 'CircleStart' declared here}}
protocol CircleEnd : CircleMiddle { def circle_end()} // expected-note{{protocol 'CircleEnd' declared here}}

struct Circle {
  def circle_start() {}
  def circle_middle() {}
  def circle_end() {}
}

def testCircular(circle: Circle) {
  // FIXME: It would be nice if this failure were suppressed because the protocols
  // have circular definitions.
  var circular : CircleStart = circle // expected-error{{type 'Circle' does not conform to protocol 'CircleStart'}}
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

// Explicit conformance via a typedef
struct X3 {
  def print() {} // expected-note{{candidate has non-matching type '() -> ()'}}
}
typealias X3a : Printable = X3
typealias X3b : FormattedPrintable = X3 // expected-error{{type 'X3b' does not conform to protocol 'FormattedPrintable'}}

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

protocol GeneratorWithAssoc {
  typealias Element
  def get() -> Element // expected-note{{protocol requires function 'get' with type '() -> Element'}}
}

struct AnIntGeneratorType : GeneratorWithAssoc {
  typealias Element = Int
  def get() -> Int {}
}

struct AnInt64GeneratorType : GeneratorWithAssoc {
  typealias Element = Int
  def get() -> Int64 {}
}

struct NotAGeneratorType : GeneratorWithAssoc { // expected-error{{type 'NotAGeneratorType' does not conform to protocol 'GeneratorWithAssoc'}}
  typealias Element = Float
  def get() -> Int {} // expected-note{{candidate has non-matching type '() -> Int'}}
}

// Okay: Infers Element == Int
struct GeneratorTypeWithInferredAssociatedTypes : GeneratorWithAssoc {
  def get() -> Int {}
}

protocol EnumerableViaGenerator {
  typealias EnumerableGeneratorTypeType : Generator // expected-note{{protocol requires nested type 'EnumerableGeneratorTypeType'}}
  def enumerate() -> EnumerableGeneratorTypeType
}

extension IntGeneratorType : EnumerableViaGenerator {
  typealias EnumerableGeneratorTypeType = IntGeneratorType
}

struct NotEnumerable : EnumerableViaGenerator { // expected-error{{type 'NotEnumerable' does not conform to protocol 'EnumerableViaGenerator'}}
  typealias EnumerableGeneratorTypeType = Int // expected-note{{possibly intended match 'EnumerableGeneratorTypeType' does not conform to 'Generator'}}
  def enumerate() -> Int {}
}

protocol GetATuple {
  typealias Tuple
  def getATuple() -> Tuple
}

struct IntStringGetter : GetATuple {
  typealias Tuple = (i: Int, s: String)
  def getATuple() -> Tuple {}
}

//===----------------------------------------------------------------------===//
// Default arguments
//===----------------------------------------------------------------------===//
// FIXME: Actually make use of default arguments, check substitutions, etc.
protocol ProtoWithDefaultArg {
  def increment(value: Int = 1)
}

struct HasNoDefaultArg : ProtoWithDefaultArg {
  def increment(_: Int) {}
}

//===----------------------------------------------------------------------===//
// Variadic function requirements
//===----------------------------------------------------------------------===//
protocol IntMaxable {
  def intmax(first: Int, rest: Int...) -> Int // expected-note{{protocol requires function 'intmax' with type '(first: Int, rest: Int...) -> Int'}} expected-note{{protocol requires function 'intmax' with type '(first: Int, rest: Int...) -> Int'}}
}

struct HasIntMax : IntMaxable {
  def intmax(first: Int, rest: Int...) -> Int {}
}

struct NotIntMax1 : IntMaxable  { // expected-error{{type 'NotIntMax1' does not conform to protocol 'IntMaxable'}}
  def intmax(first: Int, rest: Int[]) -> Int {} // expected-note{{candidate has non-matching type '(first: Int, rest: Int[]) -> Int'}}
}

struct NotIntMax2 : IntMaxable { // expected-error{{type 'NotIntMax2' does not conform to protocol 'IntMaxable'}}
  def intmax(first: Int, rest: Int) -> Int {} // expected-note{{candidate has non-matching type '(first: Int, rest: Int) -> Int'}}
}

//===----------------------------------------------------------------------===//
// 'Self' type
//===----------------------------------------------------------------------===//
protocol IsEqualComparable {
  def isEqual(other: Self) -> Bool // expected-note{{protocol requires function 'isEqual' with type '(other: WrongIsEqual) -> Bool'}}
}

struct HasIsEqual : IsEqualComparable {
  def isEqual(other: HasIsEqual) -> Bool {}
}

struct WrongIsEqual : IsEqualComparable { // expected-error{{type 'WrongIsEqual' does not conform to protocol 'IsEqualComparable'}}
  def isEqual(other: Int) -> Bool {}  // expected-note{{candidate has non-matching type '(other: Int) -> Bool'}}
}

//===----------------------------------------------------------------------===//
// Using values of existential type.
//===----------------------------------------------------------------------===//

def existentialEnumerable(e: Enumerable) {
  // FIXME: Need a more specific diagnostic here.
  var x = e.enumerate() // expected-error{{'Enumerable' does not have a member named 'enumerate'}}
  x.next()
  x.nonexistent()
}

protocol HasEnumerableAndGenerator {
  typealias R : Generator, Enumerable
  def getR() -> R
}

def existentialEnumerableAndGeneratorType(h: HasEnumerableAndGenerator) {
  // FIXME: Crummy diagnostics.
  var x = h.getR() // expected-error{{'HasEnumerableAndGenerator' does not have a member named 'getR'}}
  x.enumerate()
  x.next()

  x.nonexistent()
}

//===----------------------------------------------------------------------===//
// Subscripting
//===----------------------------------------------------------------------===//
protocol IntIntSubscriptable {
  subscript (i: Int) -> Int
}

protocol IntSubscriptable {
  typealias Element
  subscript (i: Int) -> Element
}

struct DictionaryIntInt {
  subscript (i: Int) -> Int {
    get: return i
  }
}

def testSubscripting(iis: IntIntSubscriptable, i_s: IntSubscriptable) {
  var i: Int = iis[17]
  // FIXME: Crummy diagnostics.
  var i2 = i_s[17] // expected-error{{'IntSubscriptable' does not have a member named 'subscript'}}
}

//===----------------------------------------------------------------------===//
// Static methods
//===----------------------------------------------------------------------===//
protocol StaticP {
  static def f()
}
protocol InstanceP {
  def f() // expected-note{{protocol requires function 'f' with type '() -> ()'}}
}
struct StaticS1 : StaticP {
  static def f() {}
}
struct StaticS2 : InstanceP { // expected-error{{type 'StaticS2' does not conform to protocol 'InstanceP'}}
  static def f() {} // expected-note{{candidate is 'static', but requirement is not}}
}
struct StaticAndInstanceS : InstanceP {
  static def f() {}
  def f() {}
}
def StaticProtocolFunc() {
  var a: StaticP = StaticS1()
  a.f() // expected-error{{'StaticP' does not have a member named 'f'}}
}
def StaticProtocolGenericFunc<t : StaticP>() {
  t.f()
}

//===----------------------------------------------------------------------===//
// Operators
//===----------------------------------------------------------------------===//
protocol Eq {
  def ==(lhs: Self, rhs: Self) -> Bool
}

extension Int : Eq { }

// Matching prefix/postfix.
operator prefix <> {}
operator postfix <> {}

protocol IndexValue {
  @prefix def <> (max: Self) -> Int
  @postfix def <> (min: Self) -> Int
}

@prefix def <> (max: Int) -> Int  { return 0 }
@postfix def <> (min: Int) -> Int  { return 0 }

extension Int : IndexValue {}

//===----------------------------------------------------------------------===//
// Class protocols
//===----------------------------------------------------------------------===//

@class_protocol protocol IntrusiveListNode {
  var next : Self
}

class ClassNode : IntrusiveListNode {
  var next : ClassNode
}

struct StructNode : IntrusiveListNode { // expected-error{{non-class type 'StructNode' cannot conform to class protocol 'IntrusiveListNode'}}
  var next : StructNode
}

class ClassNodeByExtension { }
struct StructNodeByExtension { }

extension ClassNodeByExtension : IntrusiveListNode {
  var next : ClassNodeByExtension { get: return self; set: }
}

extension StructNodeByExtension : IntrusiveListNode { // expected-error{{non-class type 'StructNodeByExtension' cannot conform to class protocol 'IntrusiveListNode'}}
  var next : StructNodeByExtension { get: return self; set: }
}

class GenericClassNode<T> : IntrusiveListNode {
  var next : GenericClassNode<T>
}

struct GenericStructNode<T> : IntrusiveListNode { // expected-error{{non-class type 'GenericStructNode<T>' cannot conform to class protocol 'IntrusiveListNode'}}
  var next : GenericStructNode<T>
}

// Refined protocols inherit class-ness
protocol IntrusiveDListNode : IntrusiveListNode { // expected-note{{type 'StructDNode' does not conform to inherited protocol 'IntrusiveListNode'}}
  var prev : Self
}

class ClassDNode : IntrusiveDListNode {
  var prev : ClassDNode
  var next : ClassDNode
}

struct StructDNode : IntrusiveDListNode { // expected-error{{non-class type 'StructDNode' cannot conform to class protocol 'IntrusiveListNode'}}
  var prev : StructDNode
  var next : StructDNode
}

@class_protocol @objc protocol ObjCProtocol {
  def foo() // expected-note{{protocol requires function 'foo' with type '() -> ()'}}
}
@class_protocol protocol NonObjCProtocol { //expected-note{{protocol 'NonObjCProtocol' declared here}}
  def bar()
}

class DoesntConformToObjCProtocol : ObjCProtocol { // expected-error{{type 'DoesntConformToObjCProtocol' does not conform to protocol 'ObjCProtocol'}}
}

@objc protocol ObjCProtocolRefinement : ObjCProtocol { }

@objc protocol ObjCNonObjCProtocolRefinement : NonObjCProtocol { } //expected-error{{[objc] protocol 'ObjCNonObjCProtocolRefinement' cannot refine non-[objc] protocol 'NonObjCProtocol'}}

