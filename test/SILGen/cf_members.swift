// RUN: %target-swift-frontend -emit-silgen -I %S/../IDE/Inputs/custom-modules %s | %FileCheck %s

// REQUIRES: objc_interop

import ImportAsMember

func makeMetatype() -> Struct1.Type { return Struct1.self }

// CHECK-LABEL: sil @_T010cf_members17importAsUnaryInityyF
public func importAsUnaryInit() {
  // CHECK: function_ref @CCPowerSupplyCreateDangerous : $@convention(c) () -> @owned CCPowerSupply
  var a = CCPowerSupply(dangerous: ())
  let f: () -> CCPowerSupply = CCPowerSupply.init(dangerous:)
  a = f()
}

// CHECK-LABEL: sil @_T010cf_members3foo{{[_0-9a-zA-Z]*}}F
public func foo(_ x: Double) {
// CHECK: bb0([[X:%.*]] : $Double):
  // CHECK: [[GLOBALVAR:%.*]] = global_addr @IAMStruct1GlobalVar
  // CHECK: [[ZZ:%.*]] = load [trivial] [[GLOBALVAR]]
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
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @_T010cf_members12makeMetatype{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: apply [[FN]]([[X]])
  z = makeMetatype().init(value: x)

  // CHECK: [[THUNK:%.*]] = function_ref @_T0SC7Struct1VABSd5value_tcfCTcTO
  // CHECK: [[SELF_META:%.*]] = metatype $@thin Struct1.Type
  // CHECK: [[A:%.*]] = apply [[THUNK]]([[SELF_META]])
  // CHECK: [[BORROWED_A:%.*]] = begin_borrow [[A]]
  // CHECK: [[A_COPY:%.*]] = copy_value [[BORROWED_A]]
  let a: (Double) -> Struct1 = Struct1.init(value:)
  // CHECK: apply [[A_COPY]]([[X]])
  // CHECK: end_borrow [[BORROWED_A]] from [[A]]
  z = a(x)

  // TODO: Support @convention(c) references that only capture thin metatype
  // let b: @convention(c) (Double) -> Struct1 = Struct1.init(value:)
  // z = b(x)

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1InvertInPlace
  // CHECK: apply [[FN]]([[Z]])
  z.invert()

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1Rotate : $@convention(c) (@in Struct1, Double) -> Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  // CHECK: store [[ZVAL]] to [trivial] [[ZTMP:%.*]] :
  // CHECK: apply [[FN]]([[ZTMP]], [[X]])
  z = z.translate(radians: x)

  // CHECK: [[THUNK:%.*]] = function_ref [[THUNK_NAME:@_T0SC7Struct1V9translateABSd7radians_tFTcTO]]
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  // CHECK: [[C:%.*]] = apply [[THUNK]]([[ZVAL]])
  // CHECK: [[BORROWED_C:%.*]] = begin_borrow [[C]]
  // CHECK: [[C_COPY:%.*]] = copy_value [[BORROWED_C]]
  let c: (Double) -> Struct1 = z.translate(radians:)
  // CHECK: apply [[C_COPY]]([[X]])
  // CHECK: end_borrow [[BORROWED_C]] from [[C]]
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
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  // CHECK: apply [[FN]]([[ZVAL]], [[X]])
  z = z.scale(x)

  // CHECK: [[THUNK:%.*]] = function_ref @_T0SC7Struct1V5scaleABSdFTcTO
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  // CHECK: [[F:%.*]] = apply [[THUNK]]([[ZVAL]])
  // CHECK: [[BORROWED_F:%.*]] = begin_borrow [[F]]
  // CHECK: [[F_COPY:%.*]] = copy_value [[BORROWED_F]]
  let f = z.scale
  // CHECK: apply [[F_COPY]]([[X]])
  // CHECK: end_borrow [[BORROWED_F]] from [[F]]
  z = f(x)
  // CHECK: [[THUNK:%.*]] = function_ref @_T0SC7Struct1V5scaleABSdFTcTO
  // CHECK: thin_to_thick_function [[THUNK]]
  let g = Struct1.scale
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  z = g(z)(x)

  // TODO: If we implement SE-0042, this should directly reference the
  // underlying C function.
  // let h: @convention(c) (Struct1, Double) -> Struct1 = Struct1.scale
  // z = h(z, x)

  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  // CHECK: store [[ZVAL]] to [trivial] [[ZTMP:%.*]] :
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetRadius : $@convention(c) (@in Struct1) -> Double
  // CHECK: apply [[GET]]([[ZTMP]])
  _ = z.radius
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1SetRadius : $@convention(c) (Struct1, Double) -> ()
  // CHECK: apply [[SET]]([[ZVAL]], [[X]])
  z.radius = x

  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetAltitude : $@convention(c) (Struct1) -> Double
  // CHECK: apply [[GET]]([[ZVAL]])
  _ = z.altitude
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1SetAltitude : $@convention(c) (@inout Struct1, Double) -> ()
  // CHECK: apply [[SET]]([[Z]], [[X]])
  z.altitude = x
  
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetMagnitude : $@convention(c) (Struct1) -> Double
  // CHECK: apply [[GET]]([[ZVAL]])
  _ = z.magnitude

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1StaticMethod
  // CHECK: apply [[FN]]()
  var y = Struct1.staticMethod()
  // CHECK: [[THUNK:%.*]] = function_ref @_T0SC7Struct1V12staticMethods5Int32VyFZTcTO
  // CHECK: [[SELF:%.*]] = metatype
  // CHECK: [[I:%.*]] = apply [[THUNK]]([[SELF]])
  // CHECK: [[BORROWED_I:%.*]] = begin_borrow [[I]]
  // CHECK: [[I_COPY:%.*]] = copy_value [[BORROWED_I]]
  let i = Struct1.staticMethod
  // CHECK: apply [[I_COPY]]()
  // CHECK: end_borrow [[BORROWED_I]] from [[I]]
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

  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @_T010cf_members12makeMetatype{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1StaticGetProperty
  // CHECK: apply [[GET]]()
  _ = makeMetatype().property
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @_T010cf_members12makeMetatype{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1StaticSetProperty
  // CHECK: apply [[SET]](%{{[0-9]+}})
  makeMetatype().property = y
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @_T010cf_members12makeMetatype{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1StaticGetOnlyProperty
  // CHECK: apply [[GET]]()
  _ = makeMetatype().getOnlyProperty

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1SelfComesLast : $@convention(c) (Double, Struct1) -> ()
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
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
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[Z]]
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
// CHECK: } // end sil function '_T010cf_members3foo{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil shared [thunk] @_T0SC7Struct1VABSd5value_tcfCTO
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : $@thin Struct1.Type):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1CreateSimple
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil shared [thunk] @_T0SC7Struct1V9translateABSd7radians_tFTO
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : $Struct1):
// CHECK:         store [[SELF]] to [trivial] [[TMP:%.*]] :
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1Rotate
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[TMP]], [[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil shared [thunk] @_T0SC7Struct1V5scaleABSdFTO
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1Scale
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[SELF]], [[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil shared [thunk] @_T0SC7Struct1V12staticMethods5Int32VyFZTO
// CHECK:       bb0([[SELF:%.*]] : $@thin Struct1.Type):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1StaticMethod
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]()
// CHECK:         return [[RET]]

// CHECK-LABEL: sil shared [thunk] @_T0SC7Struct1V13selfComesLastySd1x_tFTO
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1SelfComesLast
// CHECK:         apply [[CFUNC]]([[X]], [[SELF]])

// CHECK-LABEL: sil shared [thunk] @_T0SC7Struct1V14selfComesThirdys5Int32V1a_Sf1bSd1xtFTO
// CHECK:       bb0([[X:%.*]] : $Int32, [[Y:%.*]] : $Float, [[Z:%.*]] : $Double, [[SELF:%.*]] : $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1SelfComesThird
// CHECK:         apply [[CFUNC]]([[X]], [[Y]], [[SELF]], [[Z]])

// CHECK-LABEL: sil @_T010cf_members3bar{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil @_T010cf_members28importGlobalVarsAsProperties{{[_0-9a-zA-Z]*}}F
public func importGlobalVarsAsProperties()
    -> (Double, CCPowerSupply, CCPowerSupply?) {
  // CHECK: global_addr @kCCPowerSupplyDC
  // CHECK: global_addr @kCCPowerSupplyAC
  // CHECK: global_addr @kCCPowerSupplyDefaultPower
  return (CCPowerSupply.defaultPower, CCPowerSupply.AC, CCPowerSupply.DC)
}
