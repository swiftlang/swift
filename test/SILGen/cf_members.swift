// RUN: %target-swift-frontend -emit-silgen -I %S/../IDE/Inputs/custom-modules %s | %FileCheck %s

// REQUIRES: objc_interop

import ImportAsMember

func makeMetatype() -> Struct1.Type { return Struct1.self }

// CHECK-LABEL: sil @_TF10cf_members17importAsUnaryInitFT_T_
public func importAsUnaryInit() {
  // CHECK: function_ref @CCPowerSupplyCreateDangerous : $@convention(c) () -> @owned CCPowerSupply
  var a = CCPowerSupply(dangerous: ())
  let f: () -> CCPowerSupply = CCPowerSupply.init(dangerous:)
  a = f()
}

// CHECK-LABEL: sil @_TF10cf_members3foo
public func foo(_ x: Double) {
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

  // CHECK: [[THUNK:%.*]] = function_ref @_TTOFVSC7Struct1CFT5valueSd_S_
  // CHECK: [[SELF:%.*]] = metatype $@thin Struct1.Type
  // CHECK: [[A:%.*]] = apply [[THUNK]]([[SELF]])
  let a: (Double) -> Struct1 = Struct1.init(value:)
  // CHECK: apply [[A]]([[X]])
  z = a(x)

  // TODO: Support @convention(c) references that only capture thin metatype
  // let b: @convention(c) (Double) -> Struct1 = Struct1.init(value:)
  // z = b(x)

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1InvertInPlace
  // CHECK: apply [[FN]]([[Z]])
  z.invert()

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1Rotate : $@convention(c) (@in Struct1, Double) -> Struct1
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: store [[ZVAL]] to [[ZTMP:%.*]] :
  // CHECK: apply [[FN]]([[ZTMP]], [[X]])
  z = z.translate(radians: x)

  // CHECK: [[THUNK:%.*]] = function_ref [[THUNK_NAME:@_TTOFVSC7Struct19translateFT7radiansSd_S_]]
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: [[C:%.*]] = apply [[THUNK]]([[ZVAL]])
  let c: (Double) -> Struct1 = z.translate(radians:)
  // CHECK: apply [[C]]([[X]])
  z = c(x)
  // CHECK: [[THUNK:%.*]] = function_ref [[THUNK_NAME]]
  // CHECK: thin_to_thick_function [[THUNK]]
  let d: (Struct1) -> (Double) -> Struct1 = Struct1.translate(radians:)
  z = d(z)(x)

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

  // CHECK: [[THUNK:%.*]] = function_ref @_TTOFVSC7Struct15scaleFSdS_
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: [[F:%.*]] = apply [[THUNK]]([[ZVAL]])
  let f = z.scale
  // CHECK: apply [[F]]([[X]])
  z = f(x)
  // CHECK: [[THUNK:%.*]] = function_ref @_TTOFVSC7Struct15scaleFSdS_
  // CHECK: thin_to_thick_function [[THUNK]]
  let g = Struct1.scale
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  z = g(z)(x)

  // TODO: If we implement SE-0042, this should directly reference the
  // underlying C function.
  // let h: @convention(c) (Struct1, Double) -> Struct1 = Struct1.scale
  // z = h(z, x)

  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: store [[ZVAL]] to [[ZTMP:%.*]] :
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetRadius : $@convention(c) (@in Struct1) -> Double
  // CHECK: apply [[GET]]([[ZTMP]])
  _ = z.radius
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1SetRadius : $@convention(c) (Struct1, Double) -> ()
  // CHECK: apply [[SET]]([[ZVAL]], [[X]])
  z.radius = x

  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetAltitude : $@convention(c) (Struct1) -> Double
  // CHECK: apply [[GET]]([[ZVAL]])
  _ = z.altitude
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1SetAltitude : $@convention(c) (@inout Struct1, Double) -> ()
  // CHECK: apply [[SET]]([[Z]], [[X]])
  z.altitude = x
  
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetMagnitude : $@convention(c) (Struct1) -> Double
  // CHECK: apply [[GET]]([[ZVAL]])
  _ = z.magnitude

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1StaticMethod
  // CHECK: apply [[FN]]()
  var y = Struct1.staticMethod()
  // CHECK: [[THUNK:%.*]] = function_ref @_TTOZFVSC7Struct112staticMethodFT_Vs5Int32 
  // CHECK: [[SELF:%.*]] = metatype
  // CHECK: [[I:%.*]] = apply [[THUNK]]([[SELF]])
  let i = Struct1.staticMethod
  // CHECK: apply [[I]]()
  y = i()

  // TODO: Support @convention(c) references that only capture thin metatype
  // let j: @convention(c) () -> Int32 = Struct1.staticMethod
  // y = j()

  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1StaticGetProperty
  // CHECK: apply [[GET]]()
  _ = Struct1.property
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1StaticSetProperty
  // CHECK: apply [[SET]](%{{[0-9]+}})
  Struct1.property = y
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1StaticGetOnlyProperty
  // CHECK: apply [[GET]]()
  _ = Struct1.getOnlyProperty

  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @_TF10cf_members12makeMetatype
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1StaticGetProperty
  // CHECK: apply [[GET]]()
  _ = makeMetatype().property
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @_TF10cf_members12makeMetatype
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1StaticSetProperty
  // CHECK: apply [[SET]](%{{[0-9]+}})
  makeMetatype().property = y
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @_TF10cf_members12makeMetatype
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1StaticGetOnlyProperty
  // CHECK: apply [[GET]]()
  _ = makeMetatype().getOnlyProperty

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1SelfComesLast : $@convention(c) (Double, Struct1) -> ()
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: apply [[FN]]([[X]], [[ZVAL]])
  z.selfComesLast(x: x)
  let k: (Double) -> () = z.selfComesLast(x:)
  k(x)
  let l: (Struct1) -> (Double) -> () = Struct1.selfComesLast(x:)
  l(z)(x)

  // TODO: If we implement SE-0042, this should thunk to reorder the arguments.
  // let m: @convention(c) (Struct1, Double) -> () = Struct1.selfComesLast(x:)
  // m(z, x)

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1SelfComesThird : $@convention(c) (Int32, Float, Struct1, Double) -> ()
  // CHECK: [[ZVAL:%.*]] = load [[Z]]
  // CHECK: apply [[FN]]({{.*}}, {{.*}}, [[ZVAL]], [[X]])
  z.selfComesThird(a: y, b: 0, x: x)
  let n: (Int32, Float, Double) -> () = z.selfComesThird(a:b:x:)
  n(y, 0, x)
  let o: (Struct1) -> (Int32, Float, Double) -> ()
    = Struct1.selfComesThird(a:b:x:)
  o(z)(y, 0, x)

  // TODO: If we implement SE-0042, this should thunk to reorder the arguments.
  // let p: @convention(c) (Struct1, Int, Float, Double) -> ()
  //   = Struct1.selfComesThird(a:b:x:)
  // p(z, y, 0, x)
}

// CHECK-LABEL: sil shared [thunk] @_TTOFVSC7Struct1CfT5valueSd_S_
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : $@thin Struct1.Type):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1CreateSimple
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil shared [thunk] @_TTOFVSC7Struct19translatefT7radiansSd_S_
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : $Struct1):
// CHECK:         store [[SELF]] to [[TMP:%.*]] :
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1Rotate
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[TMP]], [[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil shared [thunk] @_TTOFVSC7Struct15scalefSdS_
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1Scale
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[SELF]], [[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil shared [thunk] @_TTOZFVSC7Struct112staticMethodfT_Vs5Int32
// CHECK:       bb0([[SELF:%.*]] : $@thin Struct1.Type):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1StaticMethod
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]()
// CHECK:         return [[RET]]

// CHECK-LABEL: sil shared [thunk] @_TTOFVSC7Struct113selfComesLastfT1xSd_T_
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1SelfComesLast
// CHECK:         apply [[CFUNC]]([[X]], [[SELF]])

// CHECK-LABEL: sil shared [thunk] @_TTOFVSC7Struct114selfComesThirdfT1aVs5Int321bSf1xSd_T_
// CHECK:       bb0([[X:%.*]] : $Int32, [[Y:%.*]] : $Float, [[Z:%.*]] : $Double, [[SELF:%.*]] : $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1SelfComesThird
// CHECK:         apply [[CFUNC]]([[X]], [[Y]], [[SELF]], [[Z]])

// CHECK-LABEL: sil @_TF10cf_members3bar
public func bar(_ x: Double) {
  // CHECK: function_ref @CCPowerSupplyCreate : $@convention(c) (Double) -> @owned CCPowerSupply
  let ps = CCPowerSupply(watts: x)
  // CHECK: function_ref @CCRefrigeratorCreate : $@convention(c) (CCPowerSupply) -> @owned CCRefrigerator
  let fridge = CCRefrigerator(powerSupply: ps)
  // CHECK: function_ref @CCRefrigeratorOpen : $@convention(c) (CCRefrigerator) -> ()
  fridge.open()
  // CHECK: function_ref @CCRefrigeratorGetPowerSupply : $@convention(c) (CCRefrigerator) -> @autoreleased CCPowerSupply
  let ps2 = fridge.powerSupply
  // CHECK: function_ref @CCRefrigeratorSetPowerSupply : $@convention(c) (CCRefrigerator, CCPowerSupply) -> ()
  fridge.powerSupply = ps2

  let a: (Double) -> CCPowerSupply = CCPowerSupply.init(watts:)
  let _ = a(x)
  let b: (CCRefrigerator) -> () -> () = CCRefrigerator.open
  b(fridge)()
  let c = fridge.open
  c()
}

// CHECK-LABEL: sil @_TF10cf_members16importAsProtocolFPSo8IAMProto_T_
public func importAsProtocol(_ x: IAMProto_t) {
  // CHECK: function_ref @mutateSomeState : $@convention(c) <τ_0_0 where τ_0_0 : IAMProto> (τ_0_0) -> ()
  x.mutateSomeState()
  // CHECK: function_ref @mutateSomeStateWithParameter : $@convention(c) <τ_0_0 where τ_0_0 : IAMProto> (τ_0_0, Int) -> ()
  x.mutateSomeState(withParameter: 0)
  // CHECK: function_ref @mutateSomeStateWithFirstParameter : $@convention(c) <τ_0_0 where τ_0_0 : IAMProto> (Int, τ_0_0) -> ()
  x.mutateSomeState(withFirstParameter: 0)

  // CHECK: function_ref @getSomeValue : $@convention(c) <τ_0_0 where τ_0_0 : IAMProto> (τ_0_0) -> Int32
  let y = x.someValue
  // CHECK: function_ref @setSomeValue : $@convention(c) <τ_0_0 where τ_0_0 : IAMProto> (τ_0_0, Int32) -> Int32
  x.someValue = y
}

// CHECK-LABEL: sil @_TF10cf_members28importGlobalVarsAsProperties
public func importGlobalVarsAsProperties()
    -> (Double, CCPowerSupply, CCPowerSupply?) {
  // CHECK: global_addr @kCCPowerSupplyDC
  // CHECK: global_addr @kCCPowerSupplyAC
  // CHECK: global_addr @kCCPowerSupplyDefaultPower
  return (CCPowerSupply.defaultPower, CCPowerSupply.AC, CCPowerSupply.DC)
}
