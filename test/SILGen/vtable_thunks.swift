// RUN: %target-swift-frontend -sdk %S/Inputs -emit-silgen -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

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

  // Note: i3, i4 are implicitly @objc
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
  // Int? cannot be represented in ObjC so the override has to be
  // explicitly @nonobjc
  @nonobjc override func i3(x: Int?, y: Int) -> Int {}
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
  // Int? cannot be represented in ObjC so the override has to be
  // explicitly @nonobjc
  @nonobjc override func i3(x: Int?, y: Int) -> Int {}
  override func i4(x: Int, y: Int) -> Int {}
}

// CHECK-LABEL: sil private @_TTVFC13vtable_thunks1D3iuo
// CHECK:         [[WRAP_X:%.*]] = enum $Optional<B>
// CHECK:         [[UNWRAP_Y:%.*]] = unchecked_enum_data
// CHECK:         [[RES:%.*]] = apply {{%.*}}([[WRAP_X]], [[UNWRAP_Y]], %2, %3)
// CHECK:         [[WRAP_RES:%.*]] = enum $Optional<B>, {{.*}} [[RES]]
// CHECK:         return [[WRAP_RES]]

// CHECK-LABEL: sil private @_TTVFC13vtable_thunks1D1g
// TODO: extra copies here
// CHECK:         [[WRAPPED_X_ADDR:%.*]] = init_enum_data_addr [[WRAP_X_ADDR:%.*]] :
// CHECK:         copy_addr [take] {{%.*}} to [initialization] [[WRAPPED_X_ADDR]]
// CHECK:         inject_enum_addr [[WRAP_X_ADDR]]
// CHECK:         [[RES_ADDR:%.*]] = alloc_stack
// CHECK:         apply {{%.*}}([[RES_ADDR]], [[WRAP_X_ADDR]], %2, %3)
// CHECK:         [[DEST_ADDR:%.*]] = init_enum_data_addr %0
// CHECK:         copy_addr [take] [[RES_ADDR]] to [initialization] [[DEST_ADDR]]
// CHECK:         inject_enum_addr %0

class ThrowVariance {
  func mightThrow() throws {}
}

class NoThrowVariance: ThrowVariance {
  override func mightThrow() {}
}

// rdar://problem/20657811

class X<T: B> {
  func foo(x: T) { }
}
class Y: X<D> {
  override func foo(x: D) { }
}

// rdar://problem/21154055
// Ensure reabstraction happens when necessary to get a value in or out of an
// optional.

class Foo {
  func foo(x: @escaping (Int) -> Int) -> ((Int) -> Int)? {}
}

class Bar: Foo {
  override func foo(x: ((Int) -> Int)?) -> (Int) -> Int {}
}

// rdar://problem/21364764
// Ensure we can override an optional with an IUO or vice-versa.
struct S {}

class Aap {
  func cat(b: B?) -> B? {}
  func dog(b: B!) -> B! {}

  func catFast(s: S?) -> S? {}
  func dogFast(s: S!) -> S! {}

  func flip() -> (() -> S?) {}

  func map() -> (S) -> () -> Aap? {}
}

class Noot : Aap {
  override func cat(b: B!) -> B! {}
  override func dog(b: B?) -> B? {}

  override func catFast(s: S!) -> S! {}
  override func dogFast(s: S?) -> S? {}

  override func flip() -> (() -> S) {}

  override func map() -> (S?) -> () -> Noot {}
}

// CHECK-LABEL: sil private @_TTVFC13vtable_thunks3Bar3foo{{.*}} : $@convention(method) (@owned @callee_owned (Int) -> Int, @guaranteed Bar) -> @owned Optional<(Int) -> Int>
// CHECK:         function_ref @_TTRXFo_dSi_dSi_XFo_iSi_iSi_
// CHECK:         [[IMPL:%.*]] = function_ref @_TFC13vtable_thunks3Bar3foo{{.*}}
// CHECK:         apply [[IMPL]]
// CHECK:         function_ref @_TTRXFo_dSi_dSi_XFo_iSi_iSi_

// CHECK-LABEL: sil private @_TTVFC13vtable_thunks4Noot4flip{{.*}}
// CHECK:         [[IMPL:%.*]] = function_ref @_TFC13vtable_thunks4Noot4flip{{.*}}
// CHECK:         [[INNER:%.*]] = apply %1(%0)
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFo__dV13vtable_thunks1S_XFo__dGSqS0___
// CHECK:         [[OUTER:%.*]] = partial_apply [[THUNK]]([[INNER]])
// CHECK:         return [[OUTER]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__dV13vtable_thunks1S_XFo__dGSqS0___
// CHECK:         [[INNER:%.*]] = apply %0()
// CHECK:         [[OUTER:%.*]] = enum $Optional<S>, #Optional.some!enumelt.1, %1 : $S
// CHECK:         return [[OUTER]] : $Optional<S>

// CHECK-LABEL: sil private @_TTVFC13vtable_thunks4Noot3map{{.*}}
// CHECK:         [[IMPL:%.*]] = function_ref @_TFC13vtable_thunks4Noot3map{{.*}}
// CHECK:         [[INNER:%.*]] = apply %1(%0)
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFo_dGSqV13vtable_thunks1S__oXFo__oCS_4Noot__XFo_dS0__oXFo__oGSqCS_3Aap___
// CHECK:         [[OUTER:%.*]] = partial_apply [[THUNK]]([[INNER]])
// CHECK:         return [[OUTER]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_dGSqV13vtable_thunks1S__oXFo__oCS_4Noot__XFo_dS0__oXFo__oGSqCS_3Aap___
// CHECK:         [[ARG:%.*]] = enum $Optional<S>, #Optional.some!enumelt.1, %0
// CHECK:         [[INNER:%.*]] = apply %1(%2)
// CHECK:         [[OUTER:%.*]] = convert_function [[INNER]] : $@callee_owned () -> @owned Noot to $@callee_owned () -> @owned Optional<Aap>
// CHECK:         return [[OUTER]]
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

// CHECK-LABEL: sil_vtable NoThrowVariance {
// CHECK:         #ThrowVariance.mightThrow!1: _TF

