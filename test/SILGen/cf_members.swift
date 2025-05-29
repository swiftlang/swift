// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -I %S/../IDE/Inputs/custom-modules %s -enable-objc-interop -I %S/Inputs/usr/include | %FileCheck %s

import ImportAsMember

func makeMetatype() -> Struct1.Type { return Struct1.self }

// CHECK-LABEL: sil [ossa] @$s10cf_members17importAsUnaryInityyF :
public func importAsUnaryInit() {
  // CHECK: function_ref @CCPowerSupplyCreateDangerous : $@convention(c) () -> @owned CCPowerSupply
  var a = CCPowerSupply(dangerous: ())
  let f: (()) -> CCPowerSupply = CCPowerSupply.init(dangerous:)
  a = f(())
}
// CHECK: } // end sil function '$s10cf_members17importAsUnaryInityyF'

// CHECK-LABEL: sil [ossa] @$s10cf_members3foo{{[_0-9a-zA-Z]*}}F :
public func foo(_ x: Double) {
// CHECK: bb0([[X:%.*]] : $Double):
  // CHECK: [[GLOBALVAR:%.*]] = global_addr @IAMStruct1GlobalVar
  // CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[GLOBALVAR]] : $*Double
  // CHECK: [[ZZ:%.*]] = load [trivial] [[READ]]
  // CHECK: [[MV:%.*]] = move_value [var_decl] [[ZZ]] : $Double
  let zz = Struct1.globalVar
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[GLOBALVAR]] : $*Double
  // CHECK: assign [[MV]] to [[WRITE]]
  Struct1.globalVar = zz

  // CHECK: [[Z:%.*]] = project_box

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1CreateSimple
  // CHECK: apply [[FN]]([[X]])
  var z = Struct1(value: x)
  // The metatype expression should still be evaluated even if it isn't
  // used.
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @$s10cf_members12makeMetatype{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1CreateSimple
  // CHECK: apply [[FN]]([[X]])
  z = makeMetatype().init(value: x)

  // CHECK: [[FN:%.*]] = function_ref @$s10cf_members3fooyySdFSo10IAMStruct1VSdcfu_ : $@convention(thin) (Double) -> Struct1
  // CHECK: [[A:%.*]] = thin_to_thick_function [[FN]]
  // CHECK: [[MOVED_A:%.*]] = move_value [lexical] [var_decl] [[A]]
  // CHECK: [[BORROWED_A:%.*]] = begin_borrow [[MOVED_A]]
  // CHECK: [[COPIED_A:%.*]] = copy_value [[BORROWED_A]]
  // CHECK: [[BORROWED_A:%.*]] = begin_borrow [[COPIED_A]]
  let a: (Double) -> Struct1 = Struct1.init(value:)
  // CHECK: [[NEW_Z_VALUE:%.*]] = apply [[BORROWED_A]]([[X]])
  // CHECK: end_borrow [[BORROWED_A]]
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[Z]]
  // CHECK: assign [[NEW_Z_VALUE]] to [[WRITE]]
  // CHECK: end_access [[WRITE]]
  z = a(x)

  // TODO: Support @convention(c) references that only capture thin metatype
  // let b: @convention(c) (Double) -> Struct1 = Struct1.init(value:)
  // z = b(x)

  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[Z]] : $*Struct1
  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1InvertInPlace
  // CHECK: apply [[FN]]([[WRITE]])
  z.invert()

  // CHECK: [[WRITE:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[WRITE]]
  // CHECK: store [[ZVAL]] to [trivial] [[ZTMP:%.*]] :
  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1Rotate : $@convention(c) (@in Struct1, Double) -> Struct1
  // CHECK: apply [[FN]]([[ZTMP]], [[X]])
  z = z.translate(radians: x)

  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[THUNK:%.*]] = function_ref @$s10cf_members3fooyySdFSo10IAMStruct1VSdcADcfu0_ : $@convention(thin) (Struct1) -> @owned @callee_guaranteed (Double) -> Struct1
  // CHECK: [[C:%.*]] = apply [[THUNK]]([[ZVAL]])
  // CHECK: [[C_MOVE:%.*]] = move_value [lexical] [var_decl] [[C]]
  // CHECK: [[BORROWED_C:%.*]] = begin_borrow [[C_MOVE]]
  // CHECK: [[C_COPY:%.*]] = copy_value [[BORROWED_C]]
  // CHECK: [[BORROWED_C2:%.*]] = begin_borrow [[C_COPY]]
  let c: (Double) -> Struct1 = z.translate(radians:)
  // CHECK: apply [[BORROWED_C2]]([[X]])
  // CHECK: destroy_value [[C_COPY]]
  z = c(x)
  // CHECK: [[THUNK:%.*]] = function_ref @$s10cf_members3fooyySdFSo10IAMStruct1VSdcADcfu2_ : $@convention(thin) (Struct1) -> @owned @callee_guaranteed (Double) -> Struct1
  // CHECK: [[THICK:%.*]] = thin_to_thick_function [[THUNK]]
  // CHECK: [[MOVED_THICK:%.*]] = move_value [lexical] [var_decl] [[THICK]]
  // CHECK: [[BORROWED_THICK:%.*]] = begin_borrow [[MOVED_THICK]]
  // CHECK: [[COPIED_THICK:%.*]] = copy_value [[BORROWED_THICK]]
  let d: (Struct1) -> (Double) -> Struct1 = Struct1.translate(radians:)
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[THICK_BORROW:%.*]] = begin_borrow [[COPIED_THICK]]
  // CHECK: apply [[THICK_BORROW]]([[ZVAL]])
  // CHECK: end_borrow [[THICK_BORROW]]
  z = d(z)(x)

  // TODO: If we implement SE-0042, this should thunk the value Struct1 param
  // to a const* param to the underlying C symbol.
  //
  // let e: @convention(c) (Struct1, Double) -> Struct1
  //  = Struct1.translate(radians:)
  // z = e(z, x)

  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1Scale
  // CHECK: apply [[FN]]([[ZVAL]], [[X]])
  z = z.scale(x)

  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[THUNK:%.*]] = function_ref @$s10cf_members3fooyySdFSo10IAMStruct1VSdcADcfu4_ : $@convention(thin) (Struct1) -> @owned @callee_guaranteed (Double) -> Struct1
  // CHECK: [[F:%.*]] = apply [[THUNK]]([[ZVAL]])
  // CHECK: [[MOVED_F:%.*]] = move_value [lexical] [var_decl] [[F]]
  // CHECK: [[BORROWED_F:%.*]] = begin_borrow [[MOVED_F]]
  // CHECK: [[F_COPY:%.*]] = copy_value [[BORROWED_F]]
  // CHECK: [[BORROWED_F2:%.*]] = begin_borrow [[F_COPY]]
  let f = z.scale
  // CHECK: apply [[BORROWED_F2]]([[X]])
  // CHECK: destroy_value [[F_COPY]]
  z = f(x)
  // CHECK: [[THUNK:%.*]] = function_ref @$s10cf_members3fooyySdFSo10IAMStruct1VSdcADcfu6_ : $@convention(thin) (Struct1) -> @owned @callee_guaranteed (Double) -> Struct1
  // CHECK: thin_to_thick_function [[THUNK]]
  let g = Struct1.scale
  // CHECK:  [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  z = g(z)(x)

  // TODO: If we implement SE-0042, this should directly reference the
  // underlying C function.
  // let h: @convention(c) (Struct1, Double) -> Struct1 = Struct1.scale
  // z = h(z, x)

  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: store [[ZVAL]] to [trivial] [[ZTMP:%.*]] :
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetRadius : $@convention(c) (@in Struct1) -> Double
  // CHECK: apply [[GET]]([[ZTMP]])
  _ = z.radius
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1SetRadius : $@convention(c) (Struct1, Double) -> ()
  // CHECK: apply [[SET]]([[ZVAL]], [[X]])
  z.radius = x

  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetAltitude : $@convention(c) (Struct1) -> Double
  // CHECK: apply [[GET]]([[ZVAL]])
  _ = z.altitude
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [unknown] [[Z]] : $*Struct1
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1SetAltitude : $@convention(c) (@inout Struct1, Double) -> ()
  // CHECK: apply [[SET]]([[WRITE]], [[X]])
  z.altitude = x
  
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1GetMagnitude : $@convention(c) (Struct1) -> Double
  // CHECK: apply [[GET]]([[ZVAL]])
  _ = z.magnitude

  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1StaticMethod
  // CHECK: apply [[FN]]()
  var y = Struct1.staticMethod()
  // CHECK: [[THUNK:%.*]] = function_ref @$s10cf_members3fooyySdFs5Int32Vycfu8_ : $@convention(thin) () -> Int32 
  // CHECK: [[I2:%.*]] = thin_to_thick_function [[THUNK]]
  // CHECK: [[MOVED_I2:%.*]] = move_value [lexical] [var_decl] [[I2]]
  // CHECK: [[BORROWED_I2:%.*]] = begin_borrow [[MOVED_I2]]
  // CHECK: [[COPIED_I2:%.*]] = copy_value [[BORROWED_I2]]
  let i = Struct1.staticMethod
  // CHECK: [[BORROWED_I2:%.*]] = begin_borrow [[COPIED_I2]]
  // CHECK: apply [[BORROWED_I2]]()
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

  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @$s10cf_members12makeMetatype{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1StaticGetProperty
  // CHECK: apply [[GET]]()
  _ = makeMetatype().property
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @$s10cf_members12makeMetatype{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[SET:%.*]] = function_ref @IAMStruct1StaticSetProperty
  // CHECK: apply [[SET]](%{{[0-9]+}})
  makeMetatype().property = y
  // CHECK: [[MAKE_METATYPE:%.*]] = function_ref @$s10cf_members12makeMetatype{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[MAKE_METATYPE]]()
  // CHECK: [[GET:%.*]] = function_ref @IAMStruct1StaticGetOnlyProperty
  // CHECK: apply [[GET]]()
  _ = makeMetatype().getOnlyProperty

  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1SelfComesLast : $@convention(c) (Double, Struct1) -> ()
  // CHECK: apply [[FN]]([[X]], [[ZVAL]])
  z.selfComesLast(x: x)
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  let k: (Double) -> () = z.selfComesLast(x:)
  k(x)
  let l: (Struct1) -> (Double) -> () = Struct1.selfComesLast(x:)
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  l(z)(x)

  // TODO: If we implement SE-0042, this should thunk to reorder the arguments.
  // let m: @convention(c) (Struct1, Double) -> () = Struct1.selfComesLast(x:)
  // m(z, x)

  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[Z]] : $*Struct1
  // CHECK: [[ZVAL:%.*]] = load [trivial] [[READ]]
  // CHECK: [[FN:%.*]] = function_ref @IAMStruct1SelfComesThird : $@convention(c) (Int32, Float, Struct1, Double) -> ()
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
// CHECK: } // end sil function '$s10cf_members3foo{{[_0-9a-zA-Z]*}}F'

// CHECK-LABEL: sil private [ossa] @$s10cf_members3fooyySdFSo10IAMStruct1VSdcfu_ : $@convention(thin) (Double) -> Struct1 {
// CHECK:       bb0([[X:%.*]] : $Double):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1CreateSimple
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil private [ossa] @$s10cf_members3fooyySdFSo10IAMStruct1VSdcADcfu0_ADSdcfu1_ : $@convention(thin) (Double, Struct1) -> Struct1 {
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : @closureCapture $Struct1):
// CHECK:         store [[SELF]] to [trivial] [[TMP:%.*]] :
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1Rotate
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[TMP]], [[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil private [ossa] @$s10cf_members3fooyySdFSo10IAMStruct1VSdcADcfu4_ADSdcfu5_ : $@convention(thin) (Double, Struct1) -> Struct1 {
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : @closureCapture $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1Scale
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]([[SELF]], [[X]])
// CHECK:         return [[RET]]

// CHECK-LABEL: sil private [ossa] @$s10cf_members3fooyySdFs5Int32Vycfu8_ : $@convention(thin) () -> Int32 
// CHECK:       bb0:
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1StaticMethod
// CHECK:         [[RET:%.*]] = apply [[CFUNC]]()
// CHECK:         return [[RET]]

// CHECK-LABEL:sil private [ossa] @$s10cf_members3fooyySdFySdcSo10IAMStruct1Vcfu11_ySdcfu12_ : $@convention(thin) (Double, Struct1) -> () {
// CHECK:       bb0([[X:%.*]] : $Double, [[SELF:%.*]] : @closureCapture $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1SelfComesLast
// CHECK:         apply [[CFUNC]]([[X]], [[SELF]])

// CHECK-LABEL: sil private [ossa] @$s10cf_members3fooyySdFys5Int32V_SfSdtcSo10IAMStruct1Vcfu13_yAD_SfSdtcfu14_ : $@convention(thin) (Int32, Float, Double, Struct1) -> () {
// CHECK:       bb0([[X:%.*]] : $Int32, [[Y:%.*]] : $Float, [[Z:%.*]] : $Double, [[SELF:%.*]] : @closureCapture $Struct1):
// CHECK:         [[CFUNC:%.*]] = function_ref @IAMStruct1SelfComesThird
// CHECK:         apply [[CFUNC]]([[X]], [[Y]], [[SELF]], [[Z]])

// CHECK-LABEL: sil [ossa] @$s10cf_members3bar{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil [ossa] @$s10cf_members28importGlobalVarsAsProperties{{[_0-9a-zA-Z]*}}F
public func importGlobalVarsAsProperties()
    -> (Double, CCPowerSupply, CCPowerSupply?) {
  // CHECK: global_addr @kCCPowerSupplyDC
  // CHECK: global_addr @kCCPowerSupplyAC
  // CHECK: global_addr @kCCPowerSupplyDefaultPower
  return (CCPowerSupply.defaultPower, CCPowerSupply.AC, CCPowerSupply.DC)
}
