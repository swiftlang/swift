// RUN: %target-typecheck-verify-swift

infix operator +++

protocol ConcatToAnything {
  static func +++ <T>(lhs: Self, other: T)
}

func min<T : Comparable>(_ x: T, y: T) -> T {
  if y < x { return y }
  return x
}

func weirdConcat<T : ConcatToAnything, U>(_ t: T, u: U) {
  t +++ u
  t +++ 1
  u +++ t // expected-error{{argument type 'U' does not conform to expected type 'ConcatToAnything'}}
}

// Make sure that the protocol operators don't get in the way.
var b1, b2 : Bool
_ = b1 != b2

extension UnicodeScalar {
  func isAlpha2() -> Bool {
    return (self >= "A" && self <= "Z") || (self >= "a" && self <= "z")
  }
}

protocol P {
  static func foo(_ arg: Self) -> Self
}
struct S : P {
  static func foo(_ arg: S) -> S {
    return arg
  }
}

func foo<T : P>(_ arg: T) -> T {
  return T.foo(arg)
}

// Associated types and metatypes
protocol SomeProtocol {
  associatedtype SomeAssociated
}

func generic_metatypes<T : SomeProtocol>(_ x: T)
  -> (T.Type, T.SomeAssociated.Type)
{
  return (type(of: x), type(of: x).SomeAssociated.self)
}

// Inferring a variable's type from a call to a generic.
struct Pair<T, U> { } // expected-note 4 {{'T' declared as parameter to type 'Pair'}} expected-note {{'U' declared as parameter to type 'Pair'}}

func pair<T, U>(_ x: T, _ y: U) -> Pair<T, U> { }

var i : Int, f : Float
var p = pair(i, f)

// Conformance constraints on static variables.
func f1<S1 : Sequence>(_ s1: S1) {}
var x : Array<Int> = [1]
f1(x)

// Inheritance involving generics and non-generics.
class X {
  func f() {}
}

class Foo<T> : X { 
  func g() { }
}

class Y<U> : Foo<Int> {
}

func genericAndNongenericBases(_ x: Foo<Int>, y: Y<()>) {
  x.f()
  y.f()
  y.g()
}

func genericAndNongenericBasesTypeParameter<T : Y<()>>(_ t: T) {
  t.f()
  t.g()
}

protocol P1 {}
protocol P2 {}

func foo<T : P1>(_ t: T) -> P2 {
  return t // expected-error{{return expression of type 'T' does not conform to 'P2'}}
}

func foo2(_ p1: P1) -> P2 {
  return p1 // expected-error{{return expression of type 'P1' does not conform to 'P2'}}
}

// <rdar://problem/14005696>
protocol BinaryMethodWorkaround {
  associatedtype MySelf
}

protocol Squigglable : BinaryMethodWorkaround {
}

infix operator ~~~

func ~~~ <T : Squigglable>(lhs: T, rhs: T) -> Bool where T.MySelf == T {
  return true
}

extension UInt8 : Squigglable {
  typealias MySelf = UInt8
}

var rdar14005696 : UInt8
_ = rdar14005696 ~~~ 5

// <rdar://problem/15168483>
public struct SomeIterator<C: Collection, Indices: Sequence>
  : IteratorProtocol, Sequence
  where C.Index == Indices.Iterator.Element {
  var seq : C
  var indices : Indices.Iterator

  public typealias Element = C.Iterator.Element

  public mutating func next() -> Element? {
    fatalError()
  }

  public init(elements: C, indices: Indices) {
    fatalError()
  }
}
func f1<T>(seq: Array<T>) {
  let x = (seq.indices).lazy.reversed()
  SomeIterator(elements: seq, indices: x) // expected-warning{{unused}}
  SomeIterator(elements: seq, indices: seq.indices.reversed()) // expected-warning{{unused}}
}


// <rdar://problem/16078944>
func count16078944<T>(_ x: Range<T>) -> Int { return 0 }

func test16078944 <T: Comparable>(lhs: T, args: T) -> Int {
  return count16078944(lhs..<args) // don't crash
}


// <rdar://problem/22409190> QoI: Passing unsigned integer to ManagedBuffer elements.destroy()
class r22409190ManagedBuffer<Value, Element> {
  final var value: Value { get {} set {}}
  func withUnsafeMutablePointerToElements<R>(
    _ body: (UnsafeMutablePointer<Element>) -> R) -> R {
  }
}
class MyArrayBuffer<Element>: r22409190ManagedBuffer<UInt, Element> {
  deinit {
    self.withUnsafeMutablePointerToElements { elems -> Void in
      elems.deinitialize(count: self.value)  // expected-error {{cannot convert value of type 'UInt' to expected argument type 'Int'}}
    }
  }
}

// <rdar://problem/22459135> error: 'print' is unavailable: Please wrap your tuple argument in parentheses: 'print((...))'
func r22459135() {
  func h<S : Sequence>(_ sequence: S) -> S.Iterator.Element
    where S.Iterator.Element : FixedWidthInteger {
    return 0
  }

  func g(_ x: Any) {}
  func f(_ x: Int) {
    g(h([3]))
  }

  func f2<TargetType: AnyObject>(_ target: TargetType, handler: @escaping (TargetType) -> ()) {
    let _: (AnyObject) -> () = { internalTarget in
      handler(internalTarget as! TargetType)
    }
  }
}


// <rdar://problem/19710848> QoI: Friendlier error message for "[] as Set"
// <rdar://problem/22326930> QoI: "argument for generic parameter 'Element' could not be inferred" lacks context
_ = [] as Set  // expected-error {{generic parameter 'Element' could not be inferred in cast to 'Set<_>}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{14-14=<<#Element: Hashable#>>}}


//<rdar://problem/22509125> QoI: Error when unable to infer generic archetype lacks greatness
func r22509125<T>(_ a : T?) { // expected-note {{in call to function 'r22509125'}}
  r22509125(nil) // expected-error {{generic parameter 'T' could not be inferred}}
}


// <rdar://problem/24267414> QoI: error: cannot convert value of type 'Int' to specified type 'Int'
struct R24267414<T> {  // expected-note {{'T' declared as parameter to type 'R24267414'}}
  static func foo() -> Int {}
}
var _ : Int = R24267414.foo() // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{24-24=<Any>}}


// https://bugs.swift.org/browse/SR-599
func SR599<T: FixedWidthInteger>() -> T.Type { return T.self }  // expected-note {{in call to function 'SR599'}}
_ = SR599()         // expected-error {{generic parameter 'T' could not be inferred}}




// <rdar://problem/19215114> QoI: Poor diagnostic when we are unable to infer type
protocol Q19215114 {}
protocol P19215114 {}

// expected-note @+1 {{in call to function 'body9215114'}}
func body9215114<T: P19215114, U: Q19215114>(_ t: T) -> (_ u: U) -> () {}

func test9215114<T: P19215114, U: Q19215114>(_ t: T) -> (U) -> () {
  //Should complain about not being able to infer type of U.
  let f = body9215114(t)  // expected-error {{generic parameter 'T' could not be inferred}}
  return f
}

// <rdar://problem/21718970> QoI: [uninferred generic param] cannot invoke 'foo' with an argument list of type '(Int)'
class Whatever<A: Numeric, B: Numeric> {  // expected-note 2 {{'A' declared as parameter to type 'Whatever'}}
  static func foo(a: B) {}
  
  static func bar() {}

}
Whatever.foo(a: 23) // expected-error {{generic parameter 'A' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{9-9=<<#A: Numeric#>, <#B: Numeric#>>}}

// <rdar://problem/21718955> Swift useless error: cannot invoke 'foo' with no arguments
Whatever.bar()  // expected-error {{generic parameter 'A' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{9-9=<<#A: Numeric#>, <#B: Numeric#>>}}

// <rdar://problem/27515965> Type checker doesn't enforce same-type constraint if associated type is Any
protocol P27515965 {
  associatedtype R
  func f() -> R
}

struct S27515965 : P27515965 {
  func f() -> Any { return self }
}

struct V27515965 {
  init<T : P27515965>(_ tp: T) where T.R == Float {}
}

func test(x: S27515965) -> V27515965 {
  return V27515965(x) // expected-error {{generic parameter 'T' could not be inferred}}
}

protocol BaseProto {}
protocol SubProto: BaseProto {}
@objc protocol NSCopyish {
  func copy() -> Any
}

struct FullyGeneric<Foo> {} // expected-note 6 {{'Foo' declared as parameter to type 'FullyGeneric'}} expected-note 6 {{generic type 'FullyGeneric' declared here}}

struct AnyClassBound<Foo: AnyObject> {} // expected-note {{'Foo' declared as parameter to type 'AnyClassBound'}} expected-note {{generic type 'AnyClassBound' declared here}}
struct AnyClassBound2<Foo> where Foo: AnyObject {} // expected-note {{'Foo' declared as parameter to type 'AnyClassBound2'}}

struct ProtoBound<Foo: SubProto> {} // expected-note {{'Foo' declared as parameter to type 'ProtoBound'}} expected-note {{generic type 'ProtoBound' declared here}}
struct ProtoBound2<Foo> where Foo: SubProto {} // expected-note {{'Foo' declared as parameter to type 'ProtoBound2'}}

struct ObjCProtoBound<Foo: NSCopyish> {} // expected-note {{'Foo' declared as parameter to type 'ObjCProtoBound'}} expected-note {{generic type 'ObjCProtoBound' declared here}}
struct ObjCProtoBound2<Foo> where Foo: NSCopyish {} // expected-note {{'Foo' declared as parameter to type 'ObjCProtoBound2'}}

struct ClassBound<Foo: X> {} // expected-note {{generic type 'ClassBound' declared here}}
struct ClassBound2<Foo> where Foo: X {} // expected-note {{generic type 'ClassBound2' declared here}}

struct ProtosBound<Foo> where Foo: SubProto & NSCopyish {} // expected-note {{'Foo' declared as parameter to type 'ProtosBound'}} expected-note {{generic type 'ProtosBound' declared here}}
struct ProtosBound2<Foo: SubProto & NSCopyish> {} // expected-note {{'Foo' declared as parameter to type 'ProtosBound2'}}
struct ProtosBound3<Foo: SubProto> where Foo: NSCopyish {} // expected-note {{'Foo' declared as parameter to type 'ProtosBound3'}}

struct AnyClassAndProtoBound<Foo> where Foo: AnyObject, Foo: SubProto {} // expected-note {{'Foo' declared as parameter to type 'AnyClassAndProtoBound'}}
struct AnyClassAndProtoBound2<Foo> where Foo: SubProto, Foo: AnyObject {} // expected-note {{'Foo' declared as parameter to type 'AnyClassAndProtoBound2'}}

struct ClassAndProtoBound<Foo> where Foo: X, Foo: SubProto {} // expected-note {{'Foo' declared as parameter to type 'ClassAndProtoBound'}}

struct ClassAndProtosBound<Foo> where Foo: X, Foo: SubProto, Foo: NSCopyish {} // expected-note {{'Foo' declared as parameter to type 'ClassAndProtosBound'}}
struct ClassAndProtosBound2<Foo> where Foo: X, Foo: SubProto & NSCopyish {} // expected-note {{'Foo' declared as parameter to type 'ClassAndProtosBound2'}}

extension Pair {
  init(first: T) {}
  init(second: U) {}

  var first: T { fatalError() }
  var second: U { fatalError() }
}

func testFixIts() {
  _ = FullyGeneric() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{19-19=<Any>}}
  _ = FullyGeneric<Any>()

  _ = AnyClassBound() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{20-20=<AnyObject>}}
  _ = AnyClassBound<Any>() // expected-error {{type 'Any' does not conform to protocol 'AnyObject'}}
  _ = AnyClassBound<AnyObject>()

  _ = AnyClassBound2() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{21-21=<AnyObject>}}
  _ = AnyClassBound2<Any>() // expected-error {{type 'Any' does not conform to protocol 'AnyObject'}}
  _ = AnyClassBound2<AnyObject>()

  _ = ProtoBound() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{17-17=<<#Foo: SubProto#>>}}
  _ = ProtoBound<Any>() // expected-error {{type 'Any' does not conform to protocol 'SubProto'}}

  _ = ProtoBound2() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{18-18=<<#Foo: SubProto#>>}}
  _ = ProtoBound2<Any>() // expected-error {{type 'Any' does not conform to protocol 'SubProto'}}

  _ = ObjCProtoBound() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{21-21=<NSCopyish>}}
  _ = ObjCProtoBound<AnyObject>() // expected-error {{type 'AnyObject' does not conform to protocol 'NSCopyish'}}
  _ = ObjCProtoBound<NSCopyish>()

  _ = ObjCProtoBound2() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{22-22=<NSCopyish>}}
  _ = ObjCProtoBound2<AnyObject>() // expected-error {{type 'AnyObject' does not conform to protocol 'NSCopyish'}}
  _ = ObjCProtoBound2<NSCopyish>()

  _ = ProtosBound() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{18-18=<<#Foo: NSCopyish & SubProto#>>}}
  _ = ProtosBound2() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{19-19=<<#Foo: NSCopyish & SubProto#>>}}
  _ = ProtosBound3() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{19-19=<<#Foo: NSCopyish & SubProto#>>}}

  _ = AnyClassAndProtoBound() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{28-28=<<#Foo: AnyObject & SubProto#>>}}
  _ = AnyClassAndProtoBound2() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{29-29=<<#Foo: AnyObject & SubProto#>>}}

  _ = ClassAndProtoBound() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{25-25=<<#Foo: X & SubProto#>>}}

  _ = ClassAndProtosBound() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{26-26=<<#Foo: X & NSCopyish & SubProto#>>}}
  _ = ClassAndProtosBound2() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{27-27=<<#Foo: X & NSCopyish & SubProto#>>}}

  _ = Pair() // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{11-11=<Any, Any>}}
  _ = Pair(first: S()) // expected-error {{generic parameter 'U' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{11-11=<S, Any>}}
  _ = Pair(second: S()) // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{11-11=<Any, S>}}
}

func testFixItClassBound() {
  // We infer a single class bound for simple cases in expressions...
  let x = ClassBound()
  let x1: String = x // expected-error {{cannot convert value of type 'ClassBound<X>' to specified type 'String'}}

  let y = ClassBound2()
  let y1: String = y // expected-error {{cannot convert value of type 'ClassBound2<X>' to specified type 'String'}}

  // ...but not in types.
  let z1: ClassBound // expected-error {{reference to generic type 'ClassBound' requires arguments in <...>}} {{21-21=<X>}}
  let z2: ClassBound2 // expected-error {{reference to generic type 'ClassBound2' requires arguments in <...>}} {{22-22=<X>}}
}

func testFixItCasting(x: Any) {
  _ = x as! FullyGeneric // expected-error {{generic parameter 'Foo' could not be inferred in cast to 'FullyGeneric<_>'}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{25-25=<Any>}}
}

func testFixItContextualKnowledge() {
  // FIXME: These could propagate backwards.
  let _: Int = Pair().first // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{20-20=<Any, Any>}}
  let _: Int = Pair().second // expected-error {{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{20-20=<Any, Any>}}
}

func testFixItTypePosition() {
  let _: FullyGeneric // expected-error {{reference to generic type 'FullyGeneric' requires arguments in <...>}} {{22-22=<Any>}}
  let _: ProtoBound // expected-error {{reference to generic type 'ProtoBound' requires arguments in <...>}} {{20-20=<<#Foo: SubProto#>>}}
  let _: ObjCProtoBound // expected-error {{reference to generic type 'ObjCProtoBound' requires arguments in <...>}} {{24-24=<NSCopyish>}}
  let _: AnyClassBound // expected-error {{reference to generic type 'AnyClassBound' requires arguments in <...>}} {{23-23=<AnyObject>}}
  let _: ProtosBound // expected-error {{reference to generic type 'ProtosBound' requires arguments in <...>}} {{21-21=<<#Foo: NSCopyish & SubProto#>>}}
}

func testFixItNested() {
  _ = Array<FullyGeneric>() // expected-error {{reference to generic type 'FullyGeneric' requires arguments in <...>}} {{25-25=<Any>}}
  _ = [FullyGeneric]() // expected-error {{generic parameter 'Foo' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{20-20=<Any>}}

  _ = FullyGeneric<FullyGeneric>() // expected-error {{reference to generic type 'FullyGeneric' requires arguments in <...>}} {{32-32=<Any>}}

  _ = Pair<
    FullyGeneric, // expected-error {{reference to generic type 'FullyGeneric' requires arguments in <...>}} {{17-17=<Any>}}
    FullyGeneric // FIXME: We could diagnose both of these, but we don't.
  >()
  _ = Pair<
    FullyGeneric<Any>,
    FullyGeneric // expected-error {{reference to generic type 'FullyGeneric' requires arguments in <...>}} {{17-17=<Any>}}
  >()
  _ = Pair<
    FullyGeneric, // expected-error {{reference to generic type 'FullyGeneric' requires arguments in <...>}} {{17-17=<Any>}}
    FullyGeneric<Any>
  >()

  _ = pair( // expected-error {{generic parameter 'Foo' could not be inferred}} {{none}}
    FullyGeneric(), // expected-note {{explicitly specify the generic arguments to fix this issue}}
    FullyGeneric()
  )
  _ = pair( // expected-error {{generic parameter 'Foo' could not be inferred}} {{none}}
    FullyGeneric<Any>(),
    FullyGeneric() // expected-note {{explicitly specify the generic arguments to fix this issue}}
  )
  _ = pair( // expected-error {{generic parameter 'Foo' could not be inferred}} {{none}}
    FullyGeneric(), // expected-note {{explicitly specify the generic arguments to fix this issue}}
    FullyGeneric<Any>()
  )
}

// rdar://problem/26845038
func occursCheck26845038(a: [Int]) {
  _ = Array(a)[0]
}

// rdar://problem/29633747
extension Array where Element: Hashable {
    public func trimmed(_ elements: [Element]) -> SubSequence {
        return []
    }
}

func rdar29633747(characters: String.CharacterView) {
  let _ = Array(characters).trimmed(["("])
}

// Null pointer dereference in noteArchetypeSource()
class GenericClass<A> {}
// expected-note@-1 {{'A' declared as parameter to type 'GenericClass'}}

func genericFunc<T>(t: T) {
  _ = [T: GenericClass] // expected-error {{generic parameter 'A' could not be inferred}}
  // expected-note@-1 {{explicitly specify the generic arguments to fix this issue}}
  // expected-error@-2 2 {{type 'T' does not conform to protocol 'Hashable'}}
}

struct SR_3525<T> {}
func sr3525_arg_int(_: inout SR_3525<Int>) {}
func sr3525_arg_gen<T>(_: inout SR_3525<T>) {}
func sr3525_1(t: SR_3525<Int>) {
  let _ = sr3525_arg_int(&t) // expected-error {{cannot pass immutable value as inout argument: 't' is a 'let' constant}}
}
func sr3525_2(t: SR_3525<Int>) {
  let _ = sr3525_arg_gen(&t) // expected-error {{cannot pass immutable value as inout argument: 't' is a 'let' constant}}
}
func sr3525_3<T>(t: SR_3525<T>) {
  let _ = sr3525_arg_gen(&t) // expected-error {{cannot pass immutable value as inout argument: 't' is a 'let' constant}}
}

class testStdlibType {
  let _: Array // expected-error {{reference to generic type 'Array' requires arguments in <...>}} {{15-15=<Any>}}
}
