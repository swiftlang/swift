// RUN: %target-typecheck-verify-swift

class B { 
  init() {} 
}
class D : B {
  override init() { super.init() }
}

var seven : Double = 7

var pair : (Int, Double) = (1, 2)

var closure : (Int, Int) -> Int = { $0 + $1 }

var d_as_b : B = D()
var b_as_d = B() as! D
var bad_b_as_d : D = B()  // expected-error{{cannot convert value of type 'B' to specified type 'D'}}

var d = D()
var b = B()

var d_as_b_2 : B = d 
var b_as_d_2 = b as! D

var b_is_d:Bool = B() is D
// FIXME: Poor diagnostic below.
var bad_d_is_b:Bool = D() is B // expected-warning{{always true}}

func base_class_archetype_casts<T : B>(_ t: T) {
  var _ : B = t
  _ = B() as! T
  var _ : T = B() // expected-error{{cannot convert value of type 'B' to specified type 'T'}}

  let b = B()

  _ = b as! T

  var _:Bool = B() is T
  var _:Bool = b is T
  var _:Bool = t is B // expected-warning{{always true}}

  _ = t as! D
}

protocol P1 { func p1() }
protocol P2 { func p2() }

struct S1 : P1 {
  func p1() {}
}
class C1 : P1 {
  func p1() {}
}
class D1 : C1 {}

struct S2 : P2 {
  func p2() {}
}

struct S3 {}

struct S12 : P1, P2 {
  func p1() {}
  func p2() {}
}

func protocol_archetype_casts<T : P1>(_ t: T, p1: P1, p2: P2, p12: P1 & P2) {
  // Coercions.
  var _ : P1 = t
  var _ : P2 = t // expected-error{{value of type 'T' does not conform to specified type 'P2'}}

  // Checked unconditional casts.
  _ = p1 as! T
  _ = p2 as! T
  _ = p12 as! T

  _ = t as! S1
  _ = t as! S12
  _ = t as! C1
  _ = t as! D1

  _ = t as! S2

  _ = S1() as! T
  _ = S12() as! T
  _ = C1() as! T
  _ = D1() as! T

  _ = S2() as! T

  // Type queries.
  var _:Bool = p1 is T
  var _:Bool = p2 is T
  var _:Bool = p12 is T

  var _:Bool = t is S1
  var _:Bool = t is S12
  var _:Bool = t is C1
  var _:Bool = t is D1

  var _:Bool = t is S2
}

func protocol_concrete_casts(_ p1: P1, p2: P2, p12: P1 & P2) {
  // Checked unconditional casts.
  _ = p1 as! S1
  _ = p1 as! C1
  _ = p1 as! D1
  _ = p1 as! S12

  _ = p1 as! P1 & P2

  _ = p2 as! S1

  _ = p12 as! S1
  _ = p12 as! S2
  _ = p12 as! S12
  _ = p12 as! S3

  // Type queries.
  var _:Bool = p1 is S1
  var _:Bool = p1 is C1
  var _:Bool = p1 is D1
  var _:Bool = p1 is S12

  var _:Bool = p1 is P1 & P2

  var _:Bool = p2 is S1

  var _:Bool = p12 is S1
  var _:Bool = p12 is S2
  var _:Bool = p12 is S12
  var _:Bool = p12 is S3
}

func conditional_cast(_ b: B) -> D? {
  return b as? D
}

@objc protocol ObjCProto1 {}
@objc protocol ObjCProto2 {}
protocol NonObjCProto : class {}

@objc class ObjCClass {}
class NonObjCClass {}

func objc_protocol_casts(_ op1: ObjCProto1, opn: NonObjCProto) {
  _ = ObjCClass() as! ObjCProto1
  _ = ObjCClass() as! ObjCProto2
  _ = ObjCClass() as! ObjCProto1 & ObjCProto2
  _ = ObjCClass() as! NonObjCProto
  _ = ObjCClass() as! ObjCProto1 & NonObjCProto

  _ = op1 as! ObjCProto1 & ObjCProto2
  _ = op1 as! ObjCProto2
  _ = op1 as! ObjCProto1 & NonObjCProto
  _ = opn as! ObjCProto1

  _ = NonObjCClass() as! ObjCProto1
}

func dynamic_lookup_cast(_ dl: AnyObject) {
  _ = dl as! ObjCProto1
  _ = dl as! ObjCProto2
  _ = dl as! ObjCProto1 & ObjCProto2
}

// Cast to subclass with generic parameter inference
class C2<T> : B { }
class C3<T> : C2<[T]> { 
  func f(_ x: T) { }
}
var c2i : C2<[Int]> = C3()
var c3iOpt = c2i as? C3
c3iOpt?.f(5)
var b1 = c2i is C3
var c2f: C2<Float>? = b as? C2
var c2f2: C2<[Float]>? = b as! C3


// <rdar://problem/15633178>
var f: (Float) -> Float = { $0 as Float }
var f2: (B) -> Bool = { $0 is D }

func metatype_casts<T, U>(_ b: B.Type, t:T.Type, u: U.Type) {
  _ = b is D.Type
  _ = T.self is U.Type
  _ = type(of: T.self) is U.Type.Type
  _ = type(of: b) is D.Type // expected-warning{{always fails}}
  _ = b is D.Type.Type // expected-warning{{always fails}}

}

// <rdar://problem/17017851>
func forcedDowncastToOptional(_ b: B) {
  var dOpt: D? = b as! D // expected-warning{{treating a forced downcast to 'D' as optional will never produce 'nil'}}
  // expected-note@-1{{use 'as?' to perform a conditional downcast to 'D'}}{{22-23=?}}
  // expected-note@-2{{add parentheses around the cast to silence this warning}}{{18-18=(}}{{25-25=)}}
  dOpt = b as! D // expected-warning{{treating a forced downcast to 'D' as optional will never produce 'nil'}}
  // expected-note@-1{{use 'as?' to perform a conditional downcast to 'D'}}{{14-15=?}}
  // expected-note@-2{{add parentheses around the cast to silence this warning}}{{10-10=(}}{{17-17=)}}
  dOpt = (b as! D)
  _ = dOpt
}

_ = b1 as Int    // expected-error {{cannot convert value of type 'Bool' to type 'Int' in coercion}}
_ = seven as Int // expected-error {{cannot convert value of type 'Double' to type 'Int' in coercion}}

func rdar29894174(v: B?) {
  let _ = [v].flatMap { $0 as? D }
}
