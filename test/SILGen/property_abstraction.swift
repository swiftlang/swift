// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct Int {
  mutating func foo() {}
}

struct Foo<T, U> {
  var f: (T) -> U

  var g: T
}

// CHECK-LABEL: sil hidden @_TF20property_abstraction4getF
// CHECK:         bb0([[X_ORIG:%.*]] : $Foo<Int, Int>):
// CHECK:         [[F_ORIG:%.*]] = struct_extract [[X_ORIG]] : $Foo<Int, Int>, #Foo.f
// CHECK:         strong_retain [[F_ORIG]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTR
// CHECK:         [[F_SUBST:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_ORIG]])
// CHECK:         return [[F_SUBST]]
func getF(_ x: Foo<Int, Int>) -> (Int) -> Int {
  return x.f
}

// CHECK-LABEL: sil hidden @_TF20property_abstraction4setF
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTR
// CHECK:         [[F_ORIG:%.*]] = partial_apply [[REABSTRACT_FN]]({{%.*}})
// CHECK:         [[F_ADDR:%.*]] = struct_element_addr {{%.*}} : $*Foo<Int, Int>, #Foo.f
// CHECK:         assign [[F_ORIG]] to [[F_ADDR]]
func setF(_ x: inout Foo<Int, Int>, f: @escaping (Int) -> Int) {
  x.f = f
}

func inOutFunc(_ f: inout ((Int) -> Int)) { }

// CHECK-LABEL: sil hidden @_TF20property_abstraction6inOutF
// CHECK:         [[INOUTFUNC:%.*]] = function_ref @_TF20property_abstraction9inOutFunc
// CHECK:         [[F_ADDR:%.*]] = struct_element_addr {{%.*}} : $*Foo<Int, Int>, #Foo.f
// CHECK:         [[F_SUBST_MAT:%.*]] = alloc_stack
// CHECK:         [[F_ORIG:%.*]] = load [[F_ADDR]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTR
// CHECK:         [[F_SUBST_IN:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_ORIG]])
// CHECK:         store [[F_SUBST_IN]] to [[F_SUBST_MAT]]
// CHECK:         apply [[INOUTFUNC]]([[F_SUBST_MAT]])
// CHECK:         [[F_SUBST_OUT:%.*]] = load [[F_SUBST_MAT]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_TTR
// CHECK:         [[F_ORIG:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_SUBST_OUT]])
// CHECK:         assign [[F_ORIG]] to [[F_ADDR]]
func inOutF(_ x: Foo<Int, Int>) {
  var x = x
  inOutFunc(&x.f)
}

// Don't produce a writeback for generic lvalues when there's no real
// abstraction difference. <rdar://problem/16530674>
// CHECK-LABEL: sil hidden @_TF20property_abstraction23noAbstractionDifference
func noAbstractionDifference(_ x: Foo<Int, Int>) {
  var x = x
  // CHECK: [[ADDR:%.*]] = struct_element_addr {{%.*}}, #Foo.g
  // CHECK: apply {{%.*}}([[ADDR]])
  x.g.foo()
}

protocol P {}

struct AddressOnlyLet<T> {
  let f: (T) -> T
  let makeAddressOnly: P
}

// CHECK-LABEL: sil hidden @_TF20property_abstraction34getAddressOnlyReabstractedProperty{{.*}} : $@convention(thin) (@in AddressOnlyLet<Int>) -> @owned @callee_owned (Int) -> Int
// CHECK:         [[CLOSURE_ADDR:%.*]] = struct_element_addr {{%.*}} : $*AddressOnlyLet<Int>, #AddressOnlyLet.f
// CHECK:         [[CLOSURE_ORIG:%.*]] = load [[CLOSURE_ADDR]]
// CHECK:         [[REABSTRACT:%.*]] = function_ref
// CHECK:         [[CLOSURE_SUBST:%.*]] = partial_apply [[REABSTRACT]]([[CLOSURE_ORIG]])
// CHECK:         return [[CLOSURE_SUBST]]
func getAddressOnlyReabstractedProperty(_ x: AddressOnlyLet<Int>) -> (Int) -> Int {
  return x.f
}

enum Bar<T, U> {
  case F((T) -> U)
}

func getF(_ x: Bar<Int, Int>) -> (Int) -> Int {
  switch x {
  case .F(var f):
    return f
  }
}

func makeF(_ f: @escaping (Int) -> Int) -> Bar<Int, Int> {
  return Bar.F(f)
}

struct ArrayLike<T> {
  subscript(x: ()) -> T { get {} set {} }
}

typealias Test20341012 = (title: (), action: () -> ())

struct T20341012 {
    private var options: ArrayLike<Test20341012> { get {} set {} }

    // CHECK-LABEL: sil hidden @_TFV20property_abstraction9T203410121t{{.*}}
    // CHECK:         [[TMP1:%.*]] = alloc_stack $(title: (), action: @callee_owned (@in ()) -> @out ())
    // CHECK:         apply {{.*}}<(title: (), action: () -> ())>([[TMP1]],
    mutating func t() {
        _ = self.options[].title
    }
}

class MyClass {}

// When simply assigning to a property, reabstract the r-value and assign
// to the base instead of materializing and then assigning.
protocol Factory {
  associatedtype Product
  var builder : () -> Product { get set }
}
func setBuilder<F: Factory where F.Product == MyClass>(_ factory: inout F) {
  factory.builder = { return MyClass() }
}
// CHECK: sil hidden @_TF20property_abstraction10setBuilder{{.*}} : $@convention(thin) <F where F : Factory, F.Product == MyClass> (@inout F) -> ()
// CHECK: bb0(%0 : $*F):
// CHECK:   [[FACTORY:%.*]] = alloc_box $F
// CHECK:   [[PB:%.*]] = project_box [[FACTORY]]
// CHECK:   [[F0:%.*]] = function_ref @_TFF20property_abstraction10setBuilder{{.*}} : $@convention(thin) () -> @owned MyClass
// CHECK:   [[F1:%.*]] = thin_to_thick_function [[F0]]
// CHECK:   [[SETTER:%.*]] = witness_method $F, #Factory.builder!setter.1
// CHECK:   [[REABSTRACTOR:%.*]] = function_ref @_TTR
// CHECK:   [[F2:%.*]] = partial_apply [[REABSTRACTOR]]<F>([[F1]])
// CHECK:   apply [[SETTER]]<F, MyClass>([[F2]], [[PB]])
