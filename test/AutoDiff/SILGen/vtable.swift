// RUN: %target-swift-frontend -enable-experimental-differentiable-programming -emit-silgen %s | %FileCheck %s

// Test derivative function vtable entries for `@differentiable` class members:
// - Methods.
// - Accessors (from properties and subscripts).
// - Initializers.

import _Differentiation

// Dummy `Differentiable`-conforming type.
struct DummyTangentVector: Differentiable & AdditiveArithmetic {
  // FIXME(TF-648): Dummy to make `Super.TangentVector` be nontrivial.
  var _nontrivial: [Float] = []

  static var zero: Self { Self() }
  static func + (_: Self, _: Self) -> Self { Self() }
  static func - (_: Self, _: Self) -> Self { Self() }
  typealias TangentVector = Self
}

class Super: Differentiable {
  typealias TangentVector = DummyTangentVector
  func move(along _: TangentVector) {}

  var base: Float
  // FIXME(TF-648): Dummy to make `Super.TangentVector` be nontrivial.
  var _nontrivial: [Float] = []

  init(base: Float) {
    self.base = base
  }

  @differentiable(wrt: x)
  func method(_ x: Float, _ y: Float) -> Float {
    return x
  }

  @differentiable(wrt: x where T: Differentiable)
  func genericMethod<T>(_ x: T, _ y: T) -> T {
    return x
  }

  @differentiable
  var property: Float { base }

  @differentiable(wrt: x)
  subscript(_ x: Float, _ y: Float) -> Float {
    return x
  }
}

class Sub: Super {
  override init(base: Float) {
    super.init(base: base)
  }

  // Override JVP for `method` wrt `x`.
  @derivative(of: method, wrt: x)
  @derivative(of: subscript, wrt: x)
  final func jvpMethod(_ x: Float, _ y: Float) -> (value: Float, differential: (Float) -> Float) {
    fatalError()
  }
  // Override VJP for `method` wrt `x`.
  @derivative(of: method, wrt: x)
  @derivative(of: subscript, wrt: x)
  final func vjpMethod(_ x: Float, _ y: Float) -> (value: Float, pullback: (Float) -> (Float)) {
    fatalError()
  }

  // Override derivatives for `method` wrt `x`.
  // FIXME(TF-1203): This `@differentiable` attribute should not be necessary to
  // override derivatives. Fix `derivativeFunctionRequiresNewVTableEntry` to
  // account for derived declaration `@derivative` attributes.
  @differentiable(wrt: x)
  // Add new derivatives for `method` wrt `(x, y)`.
  @differentiable(wrt: (x, y))
  override func method(_ x: Float, _ y: Float) -> Float {
    return x
  }

  // Override derivatives for `property` wrt `self`.
  @differentiable
  override var property: Float { base }
  @derivative(of: property)
  final func vjpProperty() -> (value: Float, pullback: (Float) -> TangentVector) {
    fatalError()
  }

  // Override derivatives for `subscript` wrt `x`.
  @differentiable(wrt: x)
  override subscript(_ x: Float, _ y: Float) -> Float {
    return x
  }
}

class SubSub: Sub {}

// CHECK-LABEL: sil_vtable Super {
// CHECK:   #Super.method: (Super) -> (Float, Float) -> Float : @$s6vtable5SuperC6methodyS2f_SftF
// CHECK:   #Super.method!jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable5SuperC6methodyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK:   #Super.method!vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable5SuperC6methodyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK:   #Super.genericMethod: <T> (Super) -> (T, T) -> T : @$s6vtable5SuperC13genericMethodyxx_xtlF
// CHECK:   #Super.genericMethod!jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s6vtable5SuperC13genericMethodyxx_xtlF__jvp_src_0_wrt_0_{{s|16_Differentiation}}14DifferentiableRzl_vtable_entry_thunk
// CHECK:   #Super.genericMethod!vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s6vtable5SuperC13genericMethodyxx_xtlF__vjp_src_0_wrt_0_{{s|16_Differentiation}}14DifferentiableRzl_vtable_entry_thunk
// CHECK:   #Super.property!getter: (Super) -> () -> Float : @$s6vtable5SuperC8propertySfvg
// CHECK:   #Super.property!getter.jvp.S: (Super) -> () -> Float : @AD__$s6vtable5SuperC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK:   #Super.property!getter.vjp.S: (Super) -> () -> Float : @AD__$s6vtable5SuperC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK:   #Super.subscript!getter: (Super) -> (Float, Float) -> Float : @$s6vtable5SuperCyS2f_Sftcig
// CHECK:   #Super.subscript!getter.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable5SuperCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk
// CHECK:   #Super.subscript!getter.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable5SuperCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk
// CHECK: }

// CHECK-LABEL: sil_vtable Sub {
// CHECK:   #Super.method: (Super) -> (Float, Float) -> Float : @$s6vtable3SubC6methodyS2f_SftF [override]
// CHECK:   #Super.method!jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable3SubC6methodyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK:   #Super.method!vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable3SubC6methodyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK:   #Super.genericMethod: <T> (Super) -> (T, T) -> T : @$s6vtable5SuperC13genericMethodyxx_xtlF [inherited]
// CHECK:   #Super.genericMethod!jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s6vtable5SuperC13genericMethodyxx_xtlF__jvp_src_0_wrt_0_{{s|16_Differentiation}}14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK:   #Super.genericMethod!vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s6vtable5SuperC13genericMethodyxx_xtlF__vjp_src_0_wrt_0_{{s|16_Differentiation}}14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK:   #Super.property!getter: (Super) -> () -> Float : @$s6vtable3SubC8propertySfvg [override]
// CHECK:   #Super.property!getter.jvp.S: (Super) -> () -> Float : @AD__$s6vtable3SubC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK:   #Super.property!getter.vjp.S: (Super) -> () -> Float : @AD__$s6vtable3SubC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK:   #Super.subscript!getter: (Super) -> (Float, Float) -> Float : @$s6vtable3SubCyS2f_Sftcig [override]
// CHECK:   #Super.subscript!getter.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable3SubCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK:   #Super.subscript!getter.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable3SubCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk [override]
// CHECK:   #Sub.method!jvp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s6vtable3SubC6methodyS2f_SftF__jvp_src_0_wrt_0_1_vtable_entry_thunk
// CHECK:   #Sub.method!vjp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s6vtable3SubC6methodyS2f_SftF__vjp_src_0_wrt_0_1_vtable_entry_thunk
// CHECK: }

// CHECK-LABEL: sil_vtable SubSub {
// CHECK:   #Super.method: (Super) -> (Float, Float) -> Float : @$s6vtable3SubC6methodyS2f_SftF [inherited]
// CHECK:   #Super.method!jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable3SubC6methodyS2f_SftF__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK:   #Super.method!vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable3SubC6methodyS2f_SftF__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK:   #Super.genericMethod: <T> (Super) -> (T, T) -> T : @$s6vtable5SuperC13genericMethodyxx_xtlF [inherited]
// CHECK:   #Super.genericMethod!jvp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s6vtable5SuperC13genericMethodyxx_xtlF__jvp_src_0_wrt_0_{{s|16_Differentiation}}14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK:   #Super.genericMethod!vjp.SUU.<T where T : Differentiable>: <T> (Super) -> (T, T) -> T : @AD__$s6vtable5SuperC13genericMethodyxx_xtlF__vjp_src_0_wrt_0_{{s|16_Differentiation}}14DifferentiableRzl_vtable_entry_thunk [inherited]
// CHECK:   #Super.property!getter: (Super) -> () -> Float : @$s6vtable3SubC8propertySfvg [inherited]
// CHECK:   #Super.property!getter.jvp.S: (Super) -> () -> Float : @AD__$s6vtable3SubC8propertySfvg__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK:   #Super.property!getter.vjp.S: (Super) -> () -> Float : @AD__$s6vtable3SubC8propertySfvg__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK:   #Super.subscript!getter: (Super) -> (Float, Float) -> Float : @$s6vtable3SubCyS2f_Sftcig [inherited]
// CHECK:   #Super.subscript!getter.jvp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable3SubCyS2f_Sftcig__jvp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK:   #Super.subscript!getter.vjp.SUU: (Super) -> (Float, Float) -> Float : @AD__$s6vtable3SubCyS2f_Sftcig__vjp_src_0_wrt_0_vtable_entry_thunk [inherited]
// CHECK:   #Sub.method!jvp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s6vtable3SubC6methodyS2f_SftF__jvp_src_0_wrt_0_1_vtable_entry_thunk [inherited]
// CHECK:   #Sub.method!vjp.SSU: (Sub) -> (Float, Float) -> Float : @AD__$s6vtable3SubC6methodyS2f_SftF__vjp_src_0_wrt_0_1_vtable_entry_thunk [inherited]
// CHECK: }
