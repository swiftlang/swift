// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

struct Int {
  mutating func foo() {}
}

struct Foo<T, U> {
  var f: T -> U

  var g: T
}

// CHECK-LABEL: sil hidden @_TF20property_abstraction4getF
// CHECK:         bb0([[X_ORIG:%.*]] : $Foo<Int, Int>):
// CHECK:         [[F_ORIG:%.*]] = struct_extract [[X_ORIG]] : $Foo<Int, Int>, #Foo.f
// CHECK:         strong_retain [[F_ORIG]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTRXFo_iV20property_abstraction3Int_iS0__XFo_dS0__dS0__ : $@convention(thin) (Int, @owned @callee_owned (@out Int, @in Int) -> ()) -> Int
// CHECK:         [[F_SUBST:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_ORIG]])
// CHECK:         return [[F_SUBST]]
func getF(x: Foo<Int, Int>) -> Int -> Int {
  return x.f
}

// CHECK-LABEL: sil hidden @_TF20property_abstraction4setF
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTRXFo_dV20property_abstraction3Int_dS0__XFo_iS0__iS0__ : $@convention(thin) (@out Int, @in Int, @owned @callee_owned (Int) -> Int) -> ()
// CHECK:         [[F_ORIG:%.*]] = partial_apply [[REABSTRACT_FN]]({{%.*}})
// CHECK:         [[F_ADDR:%.*]] = struct_element_addr {{%.*}} : $*Foo<Int, Int>, #Foo.f
// CHECK:         assign [[F_ORIG]] to [[F_ADDR]]
func setF(inout x: Foo<Int, Int>, f: Int -> Int) {
  x.f = f
}

func inOutFunc(inout f: (Int -> Int)) { }

// CHECK-LABEL: sil hidden @_TF20property_abstraction6inOutF
// CHECK:         [[INOUTFUNC:%.*]] = function_ref @_TF20property_abstraction9inOutFunc
// CHECK:         [[F_ADDR:%.*]] = struct_element_addr {{%.*}} : $*Foo<Int, Int>, #Foo.f
// CHECK:         [[F_SUBST_MAT:%.*]] = alloc_stack
// CHECK:         [[F_ORIG:%.*]] = load [[F_ADDR]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTRXFo_iV20property_abstraction3Int_iS0__XFo_dS0__dS0__ : $@convention(thin) (Int, @owned @callee_owned (@out Int, @in Int) -> ()) -> Int
// CHECK:         [[F_SUBST_IN:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_ORIG]])
// CHECK:         store [[F_SUBST_IN]] to [[F_SUBST_MAT]]
// CHECK:         apply [[INOUTFUNC]]([[F_SUBST_MAT]]#1)
// CHECK:         [[F_SUBST_OUT:%.*]] = load [[F_SUBST_MAT]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTRXFo_dV20property_abstraction3Int_dS0__XFo_iS0__iS0__ : $@convention(thin) (@out Int, @in Int, @owned @callee_owned (Int) -> Int) -> ()
// CHECK:         [[F_ORIG:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_SUBST_OUT]])
// CHECK:         assign [[F_ORIG]] to [[F_ADDR]]
func inOutF(var x: Foo<Int, Int>) {
  inOutFunc(&x.f)
}

// Don't produce a writeback for generic lvalues when there's no real
// abstraction difference. <rdar://problem/16530674>
// CHECK-LABEL: sil hidden @_TF20property_abstraction23noAbstractionDifference
func noAbstractionDifference(var x: Foo<Int, Int>) {
  // CHECK: [[ADDR:%.*]] = struct_element_addr {{%.*}}, #Foo.g
  // CHECK: apply {{%.*}}([[ADDR]])
  x.g.foo()
}

protocol P {}

struct AddressOnlyLet<T> {
  let f: T -> T
  let makeAddressOnly: P
}

// CHECK-LABEL: sil hidden @_TF20property_abstraction34getAddressOnlyReabstractedPropertyFGVS_14AddressOnlyLetVS_3Int_FS1_S1_ : $@convention(thin) (@in AddressOnlyLet<Int>) -> @owned @callee_owned (Int) -> Int
// CHECK:         [[CLOSURE_ADDR:%.*]] = struct_element_addr {{%.*}} : $*AddressOnlyLet<Int>, #AddressOnlyLet.f
// CHECK:         [[CLOSURE_ORIG:%.*]] = load [[CLOSURE_ADDR]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref
// CHECK:         [[CLOSURE_SUBST:%.*]] = partial_apply [[REABSTRACT]]([[CLOSURE_ORIG]])
// CHECK:         return [[CLOSURE_SUBST]]
func getAddressOnlyReabstractedProperty(x: AddressOnlyLet<Int>) -> Int -> Int {
  return x.f
}

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

struct ArrayLike<T> {
  subscript(x: ()) -> T { get {} set {} }
}

typealias Test20341012 = (title: (), action: () -> ())

struct T20341012 {
    private var options: ArrayLike<Test20341012> { get {} set {} }

    // CHECK-LABEL: sil hidden @_TFV20property_abstraction9T203410121tfRS0_FT_T_
    // CHECK:         [[TMP1:%.*]] = alloc_stack $(title: (), action: @callee_owned (@out (), @in ()) -> ())
    // CHECK:         store {{.*}} to [[TMP1]]
    mutating func t() {
        _ = self.options[].title
    }
}

class MyClass {}

// When simply assigning to a property, reabstract the r-value and assign
// to the base instead of materializing and then assigning.
protocol Factory {
  typealias Product
  var builder : () -> Product { get set }
}
func setBuilder<F: Factory where F.Product == MyClass>(inout factory: F) {
  factory.builder = { return MyClass() }
}
// CHECK: sil hidden @_TF20property_abstraction10setBuilderuRq_S_7Factoryzqq_S0_7ProductCS_7MyClass_FRq_T_ : $@convention(thin) <F where F : Factory, F.Product == MyClass> (@inout F) -> ()
// CHECK: bb0(%0 : $*F):
// CHECK:   [[FACTORY:%.*]] = alloc_box $F
// CHECK:   [[F0:%.*]] = function_ref @_TFF20property_abstraction10setBuilderuRq_S_7Factoryzqq_S0_7ProductCS_7MyClass_FRq_T_U_FT_S1_ : $@convention(thin) <τ_0_0 where τ_0_0 : Factory, τ_0_0.Product == MyClass> () -> @owned MyClass
// CHECK:   [[F1:%.*]] = partial_apply [[F0]]<F>()
// CHECK:   [[SETTER:%.*]] = witness_method $F, #Factory.builder!setter.1
// CHECK:   [[REABSTRACTOR:%.*]] = function_ref @_TTRGRq_20property_abstraction7Factoryzqq_S0_7ProductCS_7MyClass_XFo__oS1__XFo__iS1__ :
// CHECK:   [[F2:%.*]] = partial_apply [[REABSTRACTOR]]<F>([[F1]])
// CHECK:   apply [[SETTER]]<F, MyClass>([[F2]], [[FACTORY]]#1)
