// RUN: %swift -parse-stdlib -emit-silgen %s | FileCheck %s

struct Int {}

struct Foo<T, U> {
  var f: T -> U
}

// CHECK-LABEL: sil @_TF20property_abstraction4getFFT1xGVS_3FooVS_3IntS1___FS1_S1_ : $@thin (@owned Foo<Int, Int>) -> @owned @callee_owned (Int) -> Int
// CHECK:         [[F_ADDR:%.*]] = struct_element_addr {{%.*}} : $*Foo<Int, Int>, #f
// CHECK:         [[F_ORIG:%.*]] = load [[F_ADDR]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTRXFo_iV20property_abstraction3Int_iS0__XFo_dS0__dS0__ : $@thin (Int, @owned @callee_owned (@out Int, @in Int) -> ()) -> Int
// CHECK:         [[F_SUBST:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_ORIG]])
// CHECK:         return [[F_SUBST]]
func getF(x: Foo<Int, Int>) -> Int -> Int {
  return x.f
}

// CHECK-LABEL: sil @_TF20property_abstraction4setFFT1xRGVS_3FooVS_3IntS1__1fFS1_S1__T_ : $@thin (@inout Foo<Int, Int>, @owned @callee_owned (Int) -> Int) -> ()
// CHECK:         [[F_ADDR:%.*]] = struct_element_addr {{%.*}} : $*Foo<Int, Int>, #f
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTRXFo_dV20property_abstraction3Int_dS0__XFo_iS0__iS0__ : $@thin (@out Int, @in Int, @owned @callee_owned (Int) -> Int) -> ()
// CHECK:         [[F_ORIG:%.*]] = partial_apply [[REABSTRACT_FN]]({{%.*}})
// CHECK:         assign [[F_ORIG]] to [[F_ADDR]]
func setF(x: @inout Foo<Int, Int>, f: Int -> Int) {
  x.f = f
}

func inOutFunc(f: @inout (Int -> Int)) { }

// CHECK-LABEL: sil @_TF20property_abstraction6inOutFFT1xGVS_3FooVS_3IntS1___T_ : $@thin (@owned Foo<Int, Int>) -> () {
// CHECK:         [[INOUTFUNC:%.*]] = function_ref @_TF20property_abstraction9inOutFuncFT1fRFVS_3IntS0__T_ : $@thin (@inout @callee_owned (Int) -> Int) -> ()
// CHECK:         [[F_ADDR:%.*]] = struct_element_addr {{%.*}} : $*Foo<Int, Int>, #f
// CHECK:         [[F_ORIG:%.*]] = load [[F_ADDR]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTRXFo_iV20property_abstraction3Int_iS0__XFo_dS0__dS0__ : $@thin (Int, @owned @callee_owned (@out Int, @in Int) -> ()) -> Int
// CHECK:         [[F_SUBST_IN:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_ORIG]])
// CHECK:         [[F_SUBST_MAT:%.*]] = alloc_stack
// CHECK:         store [[F_SUBST_IN]] to [[F_SUBST_MAT]]
// CHECK:         apply [[INOUTFUNC]]([[F_SUBST_MAT]]#1)
// CHECK:         [[F_SUBST_OUT:%.*]] = load [[F_SUBST_MAT]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTRXFo_dV20property_abstraction3Int_dS0__XFo_iS0__iS0__ : $@thin (@out Int, @in Int, @owned @callee_owned (Int) -> Int) -> ()
// CHECK:         [[F_ORIG:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_SUBST_OUT]])
// CHECK:         assign [[F_ORIG]] to [[F_ADDR]]
func inOutF(x: Foo<Int, Int>) {
  inOutFunc(&x.f)
}

/*
 TODO
enum Bar<T, U> {
  case F(T -> U)
}

func getF(x: Bar<Int, Int>) -> Int -> Int {
  switch x {
  case .F(var f):
    return f
  }
}

func makeF(f: Int -> Int) -> Bar<Int, Int> {
  return Bar.F(f)
}

*/
