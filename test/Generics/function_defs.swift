// RUN: %target-parse-verify-swift

//===----------------------------------------------------------------------===//
// Type-check function definitions
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Basic type checking
//===----------------------------------------------------------------------===//
protocol EqualComparable {
  func isEqual(other: Self) -> Bool
}

func doCompare<T : EqualComparable, U : EqualComparable>(t1: T, t2: T, u: U) -> Bool {
  var b1 = t1.isEqual(t2)
  if b1 {
    return true;
  }

  return t1.isEqual(u) // expected-error{{cannot invoke 'isEqual' with an argument list of type '(U)'}}
}

protocol MethodLessComparable {
  func isLess(other: Self) -> Bool
}

func min<T : MethodLessComparable>(x: T, y: T) -> T {
  if (y.isLess(x)) { return y }
  return x
}

//===----------------------------------------------------------------------===//
// Interaction with existential types
//===----------------------------------------------------------------------===//

func existential<T : EqualComparable, U : EqualComparable>(t1: T, t2: T, u: U) {
  var eqComp : EqualComparable = t1 // expected-error{{protocol 'EqualComparable' can only be used as a generic constraint}}
  eqComp = u
  if t1.isEqual(eqComp) {} // expected-error{{cannot invoke 'isEqual' with an argument list of type '(EqualComparable)'}}
  if eqComp.isEqual(t2) {} // expected-error{{'EqualComparable' does not have a member named 'isEqual'}}
}

protocol OtherEqualComparable {
  func isEqual(other: Self) -> Bool
}

func otherExistential<T : EqualComparable>(t1: T) {
  var otherEqComp : OtherEqualComparable = t1 // expected-error{{type 'T' does not conform to protocol 'OtherEqualComparable'}} expected-error{{protocol 'OtherEqualComparable' can only be used as a generic constraint}}
  otherEqComp = t1 // expected-error{{cannot assign a value of type 'T' to a value of type 'OtherEqualComparable'}}

  var otherEqComp2 : OtherEqualComparable // expected-error{{protocol 'OtherEqualComparable' can only be used as a generic constraint}}
  otherEqComp2 = t1 // expected-error{{cannot assign a value of type 'T' to a value of type 'OtherEqualComparable'}}

  var everyEq : protocol<EqualComparable, OtherEqualComparable> = t1 // expected-error{{type 'T' does not conform to protocol 'OtherEqualComparable'}} expected-error{{protocol 'OtherEqualComparable' can only be used as a generic constraint}} expected-error{{protocol 'EqualComparable' can only be used as a generic constraint}}
}

protocol Runcible {
  func runce<A>(x: A)
  func spoon(x: Self)
}

func testRuncible(x: Runcible) { // expected-error{{protocol 'Runcible' can only be used as a generic constraint}}
  x.runce(5)
}

//===----------------------------------------------------------------------===//
// Overloading
//===----------------------------------------------------------------------===//

protocol Overload {
  typealias A
  typealias B
  func getA() -> A
  func getB() -> B
  func f1(_: A) -> A
  func f1(_: B) -> B
  func f2(_: Int) -> A // expected-note{{found this candidate}}
  func f2(_: Int) -> B // expected-note{{found this candidate}}
  func f3(_: Int) -> Int
  func f3(_: Float) -> Float
  func f3(_: Self) -> Self

  var prop : Self { get }
}

func testOverload<Ovl : Overload, OtherOvl : Overload>(ovl: Ovl, ovl2: Ovl,
                                                       other: OtherOvl) {
  var a = ovl.getA()
  var b = ovl.getB()

  // Overloading based on arguments
  ovl.f1(a)
  a = ovl.f1(a)
  ovl.f1(b)
  b = ovl.f1(b)

  // Overloading based on return type
  a = ovl.f2(17)
  b = ovl.f2(17)
  ovl.f2(17) // expected-error{{ambiguous use of 'f2'}}

  // Check associated types from different objects/different types.
  a = ovl2.f2(17)
  a = ovl2.f1(a)

  other.f1(a) // expected-error{{cannot invoke 'f1' with an argument list of type '(Ovl.A)'}}
  
  // Overloading based on context
  var f3i : (Int) -> Int = ovl.f3 // expected-error{{partial application of generic method is not allowed}}
  var f3f : (Float) -> Float = ovl.f3 // expected-error{{partial application of generic method is not allowed}}
  var f3ovl_1 : (Ovl) -> Ovl = ovl.f3 // expected-error{{partial application of generic method is not allowed}}
  var f3ovl_2 : (Ovl) -> Ovl = ovl2.f3 // expected-error{{partial application of generic method is not allowed}}
  var f3ovl_3 : (Ovl) -> Ovl = other.f3 // expected-error{{could not find an overload for 'f3' that accepts the supplied arguments}}

  var f3i_unbound : (Ovl) -> (Int) -> Int = Ovl.f3       // expected-error{{partial application of generic method is not allowed}}
  var f3f_unbound : (Ovl) -> (Float) -> Float = Ovl.f3   // expected-error{{partial application of generic method is not allowed}}
  var f3f_unbound2 : (OtherOvl) -> (Float) -> Float = OtherOvl.f3 // expected-error{{partial application of generic method is not allowed}}
  var f3ovl_unbound_1 : (Ovl) -> (Ovl) -> Ovl = Ovl.f3  // expected-error{{partial application of generic method is not allowed}}
  var f3ovl_unbound_2 : (OtherOvl) -> (OtherOvl) -> OtherOvl = OtherOvl.f3 // expected-error{{partial application of generic method is not allowed}}
}

//===----------------------------------------------------------------------===//
// Subscripting
//===----------------------------------------------------------------------===//
protocol Subscriptable {
  typealias Index
  typealias Value

  func getIndex() -> Index
  func getValue() -> Value

  subscript (index : Index) -> Value { get set }
}

protocol IntSubscriptable {
  typealias ElementType

  func getElement() -> ElementType

  subscript (index : Int) -> ElementType { get  }
}

func subscripting<T : protocol<Subscriptable, IntSubscriptable>>(t: T) {
  var index = t.getIndex()
  var value = t.getValue()
  var element = t.getElement()

  value = t[index]
  t[index] = value // expected-error{{cannot assign to immutable value of type 'T.Value'}}
  element = t[17]
  t[42] = element // expected-error{{cannot assign to immutable value of type 'T.ElementType'}}

  t[value] = 17 // expected-error{{cannot subscript a value of type 'T' with an index of type 'T.Value'}}
}

//===----------------------------------------------------------------------===//
// Static functions
//===----------------------------------------------------------------------===//
protocol StaticEq {
  static func isEqual(x: Self, y: Self) -> Bool
}

func staticEqCheck<T : StaticEq, U : StaticEq>(t: T, u: U) {
  if t.isEqual(t, t) { return } // expected-error{{'T' does not have a member named 'isEqual'}}

  if T.isEqual(t, y: t) { return }
  if U.isEqual(u, y: u) { return }
  T.isEqual(t, y: u) // expected-error{{cannot invoke 'isEqual' with an argument list of type '(T, y: U)'}}
}

//===----------------------------------------------------------------------===//
// Operators
//===----------------------------------------------------------------------===//
protocol Ordered {
  func <(lhs: Self, rhs: Self) -> Bool
}

func testOrdered<T : Ordered>(x: T, y: Int) {
  if y < 100 || 500 < y { return }
  if x < x { return }
}

//===----------------------------------------------------------------------===//
// Requires clauses
//===----------------------------------------------------------------------===//
func conformanceViaRequires<T 
       where T : EqualComparable, T : MethodLessComparable
     >(t1: T, t2: T) -> Bool {
  var b1 = t1.isEqual(t2)
  if b1 || t1.isLess(t2) {
    return true;
  }
}

protocol GeneratesAnElement {
  typealias Element : EqualComparable
  func generate() -> Element
}

protocol AcceptsAnElement {
  typealias Element : MethodLessComparable
  func accept(e : Element)
}

func impliedSameType<T : GeneratesAnElement where T : AcceptsAnElement>(t: T) {
  t.accept(t.generate())
  var e = t.generate(), e2 = t.generate()
  if e.isEqual(e2) || e.isLess(e2) {
    return
  }
}

protocol GeneratesAssoc1 {
  typealias Assoc1 : EqualComparable
  func get() -> Assoc1
}

protocol GeneratesAssoc2 {
  typealias Assoc2 : MethodLessComparable
  func get() -> Assoc2
}

func simpleSameType
       <T : GeneratesAssoc1, U : GeneratesAssoc2 where T.Assoc1 == U.Assoc2>
       (t: T, u: U) -> Bool
{
  return t.get().isEqual(u.get()) || u.get().isLess(t.get())
}

protocol GeneratesMetaAssoc1 {
  typealias MetaAssoc1 : GeneratesAnElement
  func get() -> MetaAssoc1
}

protocol GeneratesMetaAssoc2 {
  typealias MetaAssoc2 : AcceptsAnElement
  func get() -> MetaAssoc2
}

func recursiveSameType
       <T : GeneratesMetaAssoc1, U : GeneratesMetaAssoc2, V : GeneratesAssoc1
        where T.MetaAssoc1 == V.Assoc1, V.Assoc1 == U.MetaAssoc2>
       (t: T, u: U)
{
  t.get().accept(t.get().generate())
  var e = t.get().generate(), e2 = t.get().generate()
  if e.isEqual(e2) || e.isLess(e2) {
    return
  }
}

// <rdar://problem/13985164>
protocol P1 {
  typealias Element
}

protocol P2 {
  typealias AssocP1 : P1
  func getAssocP1() -> AssocP1
}

func beginsWith2<
     E0: P1, E1: P1
     where 
       E0.Element == E1.Element, 
       E0.Element : EqualComparable
     >(e0: E0, _ e1: E1) -> Bool
{
}

func beginsWith3<
     S0: P2, S1: P2
     where 
       S0.AssocP1.Element == S1.AssocP1.Element, 
       S1.AssocP1.Element : EqualComparable
     >(seq1: S0, _ seq2: S1) -> Bool
{
  return beginsWith2(seq1.getAssocP1(), seq2.getAssocP1())
}


// FIXME: Test same-type constraints that try to equate things we
// don't want to equate, e.g., T == U.

//===----------------------------------------------------------------------===//
// Bogus requirements
//===----------------------------------------------------------------------===//
func nonTypeReq<T where T : Wibble>(_: T) {} // expected-error{{use of undeclared type 'Wibble'}}
func badProtocolReq<T where T : Int>(_: T) {} // expected-error{{type 'T' constrained to non-protocol type 'Int'; did you mean to use '=='?}}{{31-32===}}
// expected-error@-1{{same-type requirement makes generic parameter 'T' non-generic}}

func nonTypeSameType<T where T == Wibble>(_: T) {} // expected-error{{use of undeclared type 'Wibble'}}
func nonTypeSameType2<T where Wibble == T>(_: T) {} // expected-error{{use of undeclared type 'Wibble'}}
func sameTypeEq<T where T = T>(_: T) {} // expected-error{{use '==' for same-type requirements rather than '='}} {{27-28===}}

func badTypeConformance1<T where Int : EqualComparable>() {} // expected-error{{type 'Int' in conformance requirement does not refer to a generic parameter or associated type}}

func badTypeConformance2<T where T.Blarg : EqualComparable>() { } // expected-error{{'Blarg' is not a member type of 'T'}}

func badSameType<T, U : GeneratesAnElement, V 
                 where T == U.Element, 
                          U.Element == V // expected-error{{same-type requirement makes generic parameters 'T' and 'V' equivalent}}
                 >() {} 
