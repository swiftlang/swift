// RUN: %target-swift-frontend -emit-silgen -I %S/../IDE/Inputs/custom-modules %s | FileCheck %s

import ImportAsMember

func makeMetatype() -> Struct1.Type { return Struct1.self }

// CHECK-LABEL: sil @_TF10cf_members3foo
public func foo(x: Double) {
// CHECK: bb0([[X:%.*]] : $Double):
  // CHECK: [[GLOBALVAR:%.*]] = global_addr @IAMStruct1GlobalVar
  // CHECK: [[ZZ:%.*]] = load [[GLOBALVAR]]
  let zz = Struct1.globalVar
  // CHECK: assign [[ZZ]] to [[GLOBALVAR]]
  Struct1.globalVar = zz

  // CHECK: [[Z:%.*]] = project_box

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1CreateSimple
  // CHECK: apply [[FN]]([[X]])
  var z = Struct1(value: x)
  // The metatype expression should still be evaluated even if it isn't
  // used.
  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1CreateSimple
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @_TF10cf_members12makeMetatype
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: apply [[FN]]([[X]])
  z = makeMetatype().init(value: x)

/* TODO: Partial applications
  let a: (Double) -> Struct1 = Struct1.init(value:)
  z = a(x)
 */

  // TODO: Support @convention(c) references that only capture thin metatype
  // let b: @convention(c) (Double) -> Struct1 = Struct1.init(value:)
  // z = b(x)

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1InvertInPlace
  // CHECK: apply [[FN]]([[Z]])
  z.invert()

  // C/HECK: [[FN:%.*]] = function_ref @IAMStruct1Rotate : $@convention(c) (@in Struct1, Double) -> Struct1
  // CH/ECK: [[ZVAL:%.*]] = load [[Z]]
  // CHE/CK: store [[Z]] to [[TMP:%.*]] :
  // CHEC/K: apply [[FN]]([[ZTMP]], [[X]])
  z = z.translate(radians: x)

/* TODO: Partial applications
  let c: (Double) -> Struct1 = z.translate(radians:)
  z = c(x)
  let d: (Struct1) -> (Double) -> Struct1 = Struct1.translate(radians:)
  z = d(z)(x)
 */

  // TODO: If we implement SE-0042, this should thunk the value Struct1 param
  // to a const* param to the underlying C symbol.
  //
  // let e: @convention(c) (Struct1, Double) -> Struct1
  //  = Struct1.translate(radians:)
  // z = e(z, x)

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1Scale
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: apply [[FN]]([[ZVAL]], [[X]])
  z = z.scale(x)

/* TODO: Partial applications
  let f = z.scale
  z = f(x)
  let g = Struct1.scale
  z = g(z)(x)
 */

  // TODO: If we implement SE-0042, this should directly reference the
  // underlying C function.
  // let h: @convention(c) (Struct1, Double) -> Struct1 = Struct1.scale
  // z = h(z, x)

/* TODO: properties
  _ = z.radius
  z.radius = x

  _ = z.altitude
  z.altitude = x
  
  _ = z.magnitude
 */

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1StaticMethod
  // CHECK: apply [[FN]]()
  var y = Struct1.staticMethod()
/* TODO: partial applications
  let i = Struct1.staticMethod
  y = i()
 */

  // TODO: Support @convention(c) references that only capture thin metatype
  // let j: @convention(c) () -> Int32 = Struct1.staticMethod
  // y = j()

/* TODO: properties
  _ = Struct1.property
  Struct1.property = y
  _ = Struct1.getOnlyProperty
 */

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1SelfComesLast : $@convention(c) (Double, Struct1) -> ()
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: apply [[FN]]([[X]], [[ZVAL]])
  z.selfComesLast(x: x)
/* TODO: partial applications
  let k: (Double) -> () = z.selfComesLast(x:)
  k(x)
  let l: (Struct1) -> (Double) -> () = Struct1.selfComesLast(x:)
  l(z)(x)
 */

  // TODO: If we implement SE-0042, this should thunk to reorder the arguments.
  // let m: @convention(c) (Struct1, Double) -> () = Struct1.selfComesLast(x:)
  // m(z, x)

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1SelfComesThird : $@convention(c) (Int32, Float, Struct1, Double) -> ()
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: apply [[FN]]({{.*}}, {{.*}}, [[ZVAL]], [[X]])
  z.selfComesThird(a: y, b: 0, x: x)
/* TODO: partial application
  let n: (Int32, Float, Double) -> () = z.selfComesThird(a:b:x:)
  n(y, 0, x)
  let o: (Struct1) -> (Int32, Float, Double) -> ()
    = Struct1.selfComesThird(a:b:x:)
  o(z)(y, 0, x)

  // TODO: If we implement SE-0042, this should thunk to reorder the arguments.
  // let p: @convention(c) (Struct1, Int, Float, Double) -> ()
  //   = Struct1.selfComesThird(a:b:x:)
  // p(z, y, 0, x)
 */
}
