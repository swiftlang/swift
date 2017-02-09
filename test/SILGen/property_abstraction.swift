// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

struct Int {
  mutating func foo() {}
}

struct Foo<T, U> {
  var f: (T) -> U

  var g: T
}

// CHECK-LABEL: sil hidden @_T020property_abstraction4getF{{[_0-9a-zA-Z]*}}Foo{{.*}}F : $@convention(thin) (@owned Foo<Int, Int>) -> @owned @callee_owned (Int) -> Int {
// CHECK:       bb0([[X_ORIG:%.*]] : $Foo<Int, Int>):
// CHECK:         [[BORROWED_X_ORIG:%.*]] = begin_borrow [[X_ORIG]] : $Foo<Int, Int>
// CHECK:         [[F_ORIG:%.*]] = struct_extract [[BORROWED_X_ORIG]] : $Foo<Int, Int>, #Foo.f
// CHECK:         [[F_ORIG_COPY:%.*]] = copy_value [[F_ORIG]]
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_T0{{.*}}TR :
// CHECK:         [[F_SUBST:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_ORIG_COPY]])
// CHECK:         end_borrow [[BORROWED_X_ORIG]] from [[X_ORIG]]
// CHECK:         destroy_value [[X_ORIG]]
// CHECK:         return [[F_SUBST]]
// CHECK:       } // end sil function '_T020property_abstraction4getF{{[_0-9a-zA-Z]*}}F'
func getF(_ x: Foo<Int, Int>) -> (Int) -> Int {
  return x.f
}

// CHECK-LABEL: sil hidden @_T020property_abstraction4setF{{[_0-9a-zA-Z]*}}F
// CHECK:         [[REABSTRACT_FN:%.*]] = function_ref @_T0{{.*}}TR :
// CHECK:         [[F_ORIG:%.*]] = partial_apply [[REABSTRACT_FN]]({{%.*}})
// CHECK:         [[F_ADDR:%.*]] = struct_element_addr {{%.*}} : $*Foo<Int, Int>, #Foo.f
// CHECK:         assign [[F_ORIG]] to [[F_ADDR]]
func setF(_ x: inout Foo<Int, Int>, f: @escaping (Int) -> Int) {
  x.f = f
}

func inOutFunc(_ f: inout ((Int) -> Int)) { }

// CHECK-LABEL: sil hidden @_T020property_abstraction6inOutF{{[_0-9a-zA-Z]*}}F : 
// CHECK: bb0([[ARG:%.*]] : $Foo<Int, Int>):
// CHECK:   [[XBOX:%.*]] = alloc_box ${ var Foo<Int, Int> }, var, name "x"
// CHECK:   [[XBOX_PB:%.*]] = project_box [[XBOX]] : ${ var Foo<Int, Int> }, 0
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
// CHECK:   store [[ARG_COPY]] to [init] [[XBOX_PB]]
// CHECK:   [[INOUTFUNC:%.*]] = function_ref @_T020property_abstraction9inOutFunc{{[_0-9a-zA-Z]*}}F
// CHECK:   [[F_ADDR:%.*]] = struct_element_addr [[XBOX_PB]] : $*Foo<Int, Int>, #Foo.f
// CHECK:   [[F_SUBST_MAT:%.*]] = alloc_stack
// CHECK:   [[F_ORIG:%.*]] = load [copy] [[F_ADDR]]
// CHECK:   [[REABSTRACT_FN:%.*]] = function_ref @_T0{{.*}}TR :
// CHECK:   [[F_SUBST_IN:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_ORIG]])
// CHECK:   store [[F_SUBST_IN]] to [init] [[F_SUBST_MAT]]
// CHECK:   apply [[INOUTFUNC]]([[F_SUBST_MAT]])
// CHECK:   [[F_SUBST_OUT:%.*]] = load [take] [[F_SUBST_MAT]]
// CHECK:   [[REABSTRACT_FN:%.*]] = function_ref @_T0{{.*}}TR :
// CHECK:   [[F_ORIG:%.*]] = partial_apply [[REABSTRACT_FN]]([[F_SUBST_OUT]])
// CHECK:   assign [[F_ORIG]] to [[F_ADDR]]
// CHECK:   destroy_value [[XBOX]]
// CHECK:   destroy_value [[ARG]]
// CHECK: } // end sil function '_T020property_abstraction6inOutF{{[_0-9a-zA-Z]*}}F'
func inOutF(_ x: Foo<Int, Int>) {
  var x = x
  inOutFunc(&x.f)
}

// Don't produce a writeback for generic lvalues when there's no real
// abstraction difference. <rdar://problem/16530674>
// CHECK-LABEL: sil hidden @_T020property_abstraction23noAbstractionDifference{{[_0-9a-zA-Z]*}}F
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

// CHECK-LABEL: sil hidden @_T020property_abstraction34getAddressOnlyReabstractedProperty{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@in AddressOnlyLet<Int>) -> @owned @callee_owned (Int) -> Int
// CHECK: bb0([[ARG:%.*]] : $*AddressOnlyLet<Int>):
// CHECK:   [[CLOSURE_ADDR:%.*]] = struct_element_addr {{%.*}} : $*AddressOnlyLet<Int>, #AddressOnlyLet.f
// CHECK:   [[CLOSURE_ORIG:%.*]] = load [copy] [[CLOSURE_ADDR]]
// CHECK:   [[REABSTRACT:%.*]] = function_ref
// CHECK:   [[CLOSURE_SUBST:%.*]] = partial_apply [[REABSTRACT]]([[CLOSURE_ORIG]])
// CHECK:   destroy_addr [[ARG]]
// CHECK:   return [[CLOSURE_SUBST]]
// CHECK: } // end sil function '_T020property_abstraction34getAddressOnlyReabstractedProperty{{[_0-9a-zA-Z]*}}F'
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

    // CHECK-LABEL: sil hidden @_T020property_abstraction9T20341012V1t{{[_0-9a-zA-Z]*}}F
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
// CHECK: sil hidden @_T020property_abstraction10setBuilder{{[_0-9a-zA-Z]*}}F : $@convention(thin) <F where F : Factory, F.Product == MyClass> (@inout F) -> ()
// CHECK: bb0(%0 : $*F):
// CHECK:   [[F0:%.*]] = function_ref @_T020property_abstraction10setBuilder{{[_0-9a-zA-Z]*}} : $@convention(thin) () -> @owned MyClass
// CHECK:   [[F1:%.*]] = thin_to_thick_function [[F0]]
// CHECK:   [[SETTER:%.*]] = witness_method $F, #Factory.builder!setter.1
// CHECK:   [[REABSTRACTOR:%.*]] = function_ref @_T0{{.*}}TR :
// CHECK:   [[F2:%.*]] = partial_apply [[REABSTRACTOR]]([[F1]])
// CHECK:   apply [[SETTER]]<F>([[F2]], %0)
