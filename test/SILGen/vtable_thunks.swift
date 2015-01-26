// RUN: %target-swift-frontend -sdk %S/Inputs -emit-silgen -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | FileCheck %s

protocol AddrOnly {}

@objc class B {
  // We only allow B! -> B overrides for @objc methods.
  // The IUO force-unwrap requires a thunk.
  @objc func iuo(x: B, y: B!, z: B) -> B? {}

  // f* don't require thunks, since the parameters and returns are object
  // references.
  func f(x: B, y: B) -> B? {}
  func f2(x: B, y: B) -> B? {}
  func f3(x: B, y: B) -> B {}
  func f4(x: B, y: B) -> B {}

  // Thunking monomorphic address-only params and returns
  func g(x: AddrOnly, y: AddrOnly) -> AddrOnly? {}
  func g2(x: AddrOnly, y: AddrOnly) -> AddrOnly? {}
  func g3(x: AddrOnly, y: AddrOnly) -> AddrOnly {}
  func g4(x: AddrOnly, y: AddrOnly) -> AddrOnly {}

  // Thunking polymorphic address-only params and returns
  func h<T>(x: T, y: T) -> T? {}
  func h2<T>(x: T, y: T) -> T? {}
  func h3<T>(x: T, y: T) -> T {}
  func h4<T>(x: T, y: T) -> T {}

  // Thunking value params and returns
  func i(x: Int, y: Int) -> Int? {}
  func i2(x: Int, y: Int) -> Int? {}
  func i3(x: Int, y: Int) -> Int {}
  func i4(x: Int, y: Int) -> Int {}
}

class D: B {
  override func iuo(x: B?, y: B, z: B) -> B {}

  override func f(x: B?, y: B) -> B {}
  override func f2(x: B, y: B) -> B {}
  override func f3(x: B?, y: B) -> B {}
  override func f4(x: B, y: B) -> B {}

  override func g(x: AddrOnly?, y: AddrOnly) -> AddrOnly {}
  override func g2(x: AddrOnly, y: AddrOnly) -> AddrOnly {}
  override func g3(x: AddrOnly?, y: AddrOnly) -> AddrOnly {}
  override func g4(x: AddrOnly, y: AddrOnly) -> AddrOnly {}

  override func h<U>(x: U?, y: U) -> U {}
  override func h2<U>(x: U, y: U) -> U {}
  override func h3<U>(x: U?, y: U) -> U {}
  override func h4<U>(x: U, y: U) -> U {}

  override func i(x: Int?, y: Int) -> Int {}
  override func i2(x: Int, y: Int) -> Int {}
  override func i3(x: Int?, y: Int) -> Int {}
  override func i4(x: Int, y: Int) -> Int {}
}

// Inherits the thunked impls from D
class E: D { }

// Overrides w/ its own thunked impls
class F: D {
  override func f(x: B?, y: B) -> B {}
  override func f2(x: B, y: B) -> B {}
  override func f3(x: B?, y: B) -> B {}
  override func f4(x: B, y: B) -> B {}

  override func g(x: AddrOnly?, y: AddrOnly) -> AddrOnly {}
  override func g2(x: AddrOnly, y: AddrOnly) -> AddrOnly {}
  override func g3(x: AddrOnly?, y: AddrOnly) -> AddrOnly {}
  override func g4(x: AddrOnly, y: AddrOnly) -> AddrOnly {}

  override func h<U>(x: U?, y: U) -> U {}
  override func h2<U>(x: U, y: U) -> U {}
  override func h3<U>(x: U?, y: U) -> U {}
  override func h4<U>(x: U, y: U) -> U {}

  override func i(x: Int?, y: Int) -> Int {}
  override func i2(x: Int, y: Int) -> Int {}
  override func i3(x: Int?, y: Int) -> Int {}
  override func i4(x: Int, y: Int) -> Int {}

}

// CHECK-LABEL: sil private @_TTVFC13vtable_thunks1D3iuofS0_FTGSqCS_1B_1yS1_1zS1__S1_
// CHECK:         [[WRAP_X:%.*]] = enum $Optional<B>
// CHECK:         [[FORCE_UNWRAP_FN:%.*]] = function_ref @_TFSs36_getImplicitlyUnwrappedOptionalValueU__FGSQQ__Q_
// CHECK:         apply [transparent] [[FORCE_UNWRAP_FN]]<B>([[UNWRAP_Y_ADDR:%.*]]#1,
// CHECK:         [[UNWRAP_Y:%.*]] = load [[UNWRAP_Y_ADDR]]
// CHECK:         [[RES:%.*]] = apply {{%.*}}([[WRAP_X]], [[UNWRAP_Y]], %2, %3)
// CHECK:         [[WRAP_RES:%.*]] = enum $Optional<B>, {{.*}} [[RES]]
// CHECK:         return [[WRAP_RES]]

// CHECK-LABEL: sil private @_TTVFC13vtable_thunks1D1gfS0_FTGSqPS_8AddrOnly__1yPS1___PS1__
// CHECK:         [[RES_ADDR:%.*]] = init_enum_data_addr [[WRAP_RES_ADDR:%.*]] :
// CHECK:         [[WRAPPED_X_ADDR:%.*]] = init_enum_data_addr [[WRAP_X_ADDR:%.*]] :
// CHECK:         copy_addr [take] %1 to [initialization] [[WRAPPED_X_ADDR]]
// CHECK:         inject_enum_addr [[WRAP_X_ADDR]]
// CHECK:         apply {{%.*}}([[RES_ADDR]], [[WRAP_X_ADDR]], %2, %3)
// CHECK:         inject_enum_addr [[WRAP_RES_ADDR]]

// CHECK-LABEL: sil_vtable D {
// CHECK:         #B.iuo!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f2!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f3!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f4!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.g!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.g2!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.g3!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.g4!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.h!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.h2!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.h3!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.h4!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.i!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.i2!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.i3!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.i4!1: _TF{{[A-Z0-9a-z_]*}}1D

// CHECK-LABEL: sil_vtable E {
// CHECK:         #B.iuo!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f2!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f3!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f4!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.g!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.g2!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.g3!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.g4!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.h!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.h2!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.h3!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.h4!1: _TF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.i!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.i2!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.i3!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.i4!1: _TF{{[A-Z0-9a-z_]*}}1D

// CHECK-LABEL: sil_vtable F {
// CHECK:         #B.iuo!1: _TTVF{{[A-Z0-9a-z_]*}}1D
// CHECK:         #B.f!1: _TF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.f2!1: _TF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.f3!1: _TF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.f4!1: _TF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.g!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.g2!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.g3!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.g4!1: _TF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.h!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.h2!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.h3!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.h4!1: _TF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.i!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.i2!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.i3!1: _TTVF{{[A-Z0-9a-z_]*}}1F
// CHECK:         #B.i4!1: _TF{{[A-Z0-9a-z_]*}}1F

