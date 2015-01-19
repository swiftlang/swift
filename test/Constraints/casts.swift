// RUN: %target-parse-verify-swift

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
var bad_b_as_d : D = B()  // expected-error{{}}

var d = D()
var b = B()

var d_as_b_2 : B = d 
var b_as_d_2 = b as! D

var b_is_d:Bool = B() is D
// FIXME: Poor diagnostic below.
var bad_d_is_b:Bool = D() is B // expected-warning{{always true}}

func base_class_archetype_casts<T : B>(t: T) {
  var t_as_b : B = t
  var b_as_t = B() as! T
  var bad_b_as_t : T = B() // expected-error{{}}

  var b = B()

  var b_as_t_2 = b as! T

  var b_is_t:Bool = B() is T
  var b_is_t_2:Bool = b is T
  var bad_t_is_b:Bool = t is B // expected-warning{{always true}}

  var t_as_d = t as! D
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

func protocol_archetype_casts<T : P1>(t: T, p1: P1, p2: P2, p12: protocol<P1, P2>) {
  // Coercions.
  var t_as_p1 : P1 = t
  var t_as_p2 : P2 = t // expected-error{{}}

  // Checked unconditional casts.
  var p1_as_t = p1 as! T
  var p2_as_t = p2 as! T
  var p12_as_t = p12 as! T

  var t_as_s1 = t as! S1
  var t_as_s12 = t as! S12
  var t_as_c1 = t as! C1
  var t_as_d1 = t as! D1

  var t_as_s2 = t as! S2

  var s1_as_t = S1() as! T
  var s12_as_t = S12() as! T
  var c1_as_t = C1() as! T
  var d1_as_t = D1() as! T

  var s2_as_t = S2() as! T

  // Type queries.
  var p1_is_t:Bool = p1 is T
  var p2_is_t:Bool = p2 is T
  var p12_is_t:Bool = p12 is T

  var t_is_s1:Bool = t is S1
  var t_is_s12:Bool = t is S12
  var t_is_c1:Bool = t is C1
  var t_is_d1:Bool = t is D1

  var t_is_s2:Bool = t is S2
}

func protocol_concrete_casts(p1: P1, p2: P2, p12: protocol<P1, P2>) {
  // Checked unconditional casts.
  var p1_as_s1 = p1 as! S1
  var p1_as_c1 = p1 as! C1
  var p1_as_d1 = p1 as! D1
  var p1_as_s12 = p1 as! S12

  var p1_as_p12 = p1 as! protocol<P1, P2>

  var p2_as_s1 = p2 as! S1

  var p12_as_s1 = p12 as! S1
  var p12_as_s2 = p12 as! S2
  var p12_as_s12 = p12 as! S12
  var p12_as_s3 = p12 as! S3

  // Type queries.
  var p1_is_s1:Bool = p1 is S1
  var p1_is_c1:Bool = p1 is C1
  var p1_is_d1:Bool = p1 is D1
  var p1_is_s12:Bool = p1 is S12

  var p1_is_p12:Bool = p1 is protocol<P1, P2>

  var p2_is_s1:Bool = p2 is S1

  var p12_is_s1:Bool = p12 is S1
  var p12_is_s2:Bool = p12 is S2
  var p12_is_s12:Bool = p12 is S12
  var p12_is_s3:Bool = p12 is S3
}

func conditional_cast(b: B) -> D? {
  return b as? D
}

@objc protocol ObjCProto1 {}
@objc protocol ObjCProto2 {}
protocol NonObjCProto : class {}

@objc class ObjCClass {}
class NonObjCClass {}

func objc_protocol_casts(op1: ObjCProto1, opn: NonObjCProto) {
  var p1 = ObjCClass() as! ObjCProto1
  var p2 = ObjCClass() as! ObjCProto2
  var p12 = ObjCClass() as! protocol<ObjCProto1, ObjCProto2>
  var pn = ObjCClass() as! NonObjCProto
  var p1n = ObjCClass() as! protocol<ObjCProto1, NonObjCProto>

  var op12 = op1 as! protocol<ObjCProto1, ObjCProto2>
  var op2 = op1 as! protocol<ObjCProto2>
  var op1n = op1 as! protocol<ObjCProto1, NonObjCProto>
  var opn1 = opn as! ObjCProto1

  var np1 = NonObjCClass() as! ObjCProto1
}

func dynamic_lookup_cast(dl: AnyObject) {
  var p1 = dl as! ObjCProto1
  var p2 = dl as! ObjCProto2
  var p12 = dl as! protocol<ObjCProto1, ObjCProto2>
}

// Cast to subclass with generic parameter inference
class C2<T> : B { }
class C3<T> : C2<[T]> { 
  func f(x: T) { }
}
var c2i : C2<[Int]> = C3()
var c3iOpt = c2i as? C3
c3iOpt?.f(5)
var b1 = c2i is C3
var c2f: C2<Float>? = b as? C2
var c2f2: C2<[Float]>? = b as! C3


// <rdar://problem/15633178>
var f: Float -> Float = { $0 as Float }
var f2: B -> Bool = { $0 is D }

func metatype_casts<T, U>(b: B.Type) {
  let x1 = b is D.Type
  let x2 = T.self is U.Type
  let x3 = T.self.dynamicType is U.Type.Type
  let x4 = b.dynamicType is D.Type // expected-warning{{always fails}}
  let x5 = b is D.Type.Type // expected-warning{{always fails}}

}

// <rdar://problem/17017851>
func forcedDowncastToOptional(b: B) {
  var dOpt: D? = b as! D // expected-warning{{treating a forced downcast to 'D' as optional will never produce 'nil'}}
  // expected-note@-1{{use 'as?' to perform a conditional downcast to 'D'}}{{22-22=?}}
  // expected-note@-2{{add parentheses around the cast to silence this warning}}{{18-18=(}}{{25-25=)}}
  dOpt = b as! D // expected-warning{{treating a forced downcast to 'D' as optional will never produce 'nil'}}
  // expected-note@-1{{use 'as?' to perform a conditional downcast to 'D'}}{{14-14=?}}
  // expected-note@-2{{add parentheses around the cast to silence this warning}}{{10-10=(}}{{17-17=)}}
  dOpt = (b as! D)
}

