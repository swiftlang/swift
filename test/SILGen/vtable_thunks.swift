// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -sdk %S/Inputs -emit-silgen -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

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

// This test is incorrect in semantic SIL today. But it will be fixed in
// forthcoming commits.
//
// CHECK-LABEL: sil private @_T013vtable_thunks1DC3iuo{{[_0-9a-zA-Z]*}}FTV
// CHECK: bb0([[X:%.*]] : $B, [[Y:%.*]] : $Optional<B>, [[Z:%.*]] : $B, [[W:%.*]] : $D):
// CHECK:   [[WRAP_X:%.*]] = enum $Optional<B>, #Optional.some!enumelt.1, [[X]] : $B
// CHECK:   switch_enum [[Y]] : $Optional<B>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

// CHECK: [[NONE_BB]]:
// CHECK:   [[DIAGNOSE_UNREACHABLE_FUNC:%.*]] = function_ref @_TFs30_diagnoseUnexpectedNilOptional{{.*}}
// CHECK:   apply [[DIAGNOSE_UNREACHABLE_FUNC]]
// CHECK:   unreachable

// CHECK: [[SOME_BB]]([[UNWRAP_Y:%.*]] : $B):
// CHECK:   [[THUNK_FUNC:%.*]] = function_ref @_T013vtable_thunks1DC3iuo{{.*}}
// CHECK:   [[RES:%.*]] = apply [[THUNK_FUNC]]([[WRAP_X]], [[UNWRAP_Y]], [[Z]], [[W]])
// CHECK:   [[WRAP_RES:%.*]] = enum $Optional<B>, {{.*}} [[RES]]
// CHECK:   return [[WRAP_RES]]

// CHECK-LABEL: sil private @_T013vtable_thunks1DC1g{{[_0-9a-zA-Z]*}}FTV
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

// CHECK-LABEL: sil private @_T013vtable_thunks3BarC3foo{{[_0-9a-zA-Z]*}}FTV : $@convention(method) (@owned @callee_owned (Int) -> Int, @guaranteed Bar) -> @owned Optional<@callee_owned (Int) -> Int>
// CHECK:         [[IMPL:%.*]] = function_ref @_T013vtable_thunks3BarC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[IMPL]]

// CHECK-LABEL: sil private @_T013vtable_thunks4NootC4flip{{[_0-9a-zA-Z]*}}FTV
// CHECK:         [[IMPL:%.*]] = function_ref @_T013vtable_thunks4NootC4flip{{[_0-9a-zA-Z]*}}F
// CHECK:         [[INNER:%.*]] = apply %1(%0)
// CHECK:         [[THUNK:%.*]] = function_ref @_T013vtable_thunks1SVIxd_ACSgIxd_TR
// CHECK:         [[OUTER:%.*]] = partial_apply [[THUNK]]([[INNER]])
// CHECK:         return [[OUTER]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T013vtable_thunks1SVIxd_ACSgIxd_TR
// CHECK:         [[INNER:%.*]] = apply %0()
// CHECK:         [[OUTER:%.*]] = enum $Optional<S>, #Optional.some!enumelt.1, %1 : $S
// CHECK:         return [[OUTER]] : $Optional<S>

// CHECK-LABEL: sil private @_T013vtable_thunks4NootC3map{{[_0-9a-zA-Z]*}}FTV
// CHECK:         [[IMPL:%.*]] = function_ref @_T013vtable_thunks4NootC3map{{[_0-9a-zA-Z]*}}F
// CHECK:         [[INNER:%.*]] = apply %1(%0)
// CHECK:         [[THUNK:%.*]] = function_ref @_T013vtable_thunks1SVSgAA4NootCIxo_Ixyo_AcA3AapCSgIxo_Ixyo_TR
// CHECK:         [[OUTER:%.*]] = partial_apply [[THUNK]]([[INNER]])
// CHECK:         return [[OUTER]]

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T013vtable_thunks1SVSgAA4NootCIxo_Ixyo_AcA3AapCSgIxo_Ixyo_TR
// CHECK:         [[ARG:%.*]] = enum $Optional<S>, #Optional.some!enumelt.1, %0
// CHECK:         [[INNER:%.*]] = apply %1(%2)
// CHECK:         [[OUTER:%.*]] = convert_function [[INNER]] : $@callee_owned () -> @owned Noot to $@callee_owned () -> @owned Optional<Aap>
// CHECK:         return [[OUTER]]
// CHECK-LABEL: sil_vtable D {
// CHECK:         #B.iuo!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.f!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f2!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f3!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f4!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.g!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g2!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g3!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g4!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.h!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h2!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h3!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h4!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.i!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i2!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i3!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i4!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F

// CHECK-LABEL: sil_vtable E {
// CHECK:         #B.iuo!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.f!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f2!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f3!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f4!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.g!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g2!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g3!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g4!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.h!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h2!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h3!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h4!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.i!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i2!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i3!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i4!1: {{.*}} : _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}F

// CHECK-LABEL: sil_vtable F {
// CHECK:         #B.iuo!1: {{.*}} : hidden _T013vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.f!1: {{.*}} : _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f2!1: {{.*}} : _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f3!1: {{.*}} : _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f4!1: {{.*}} : _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.g!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g2!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g3!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g4!1: {{.*}} : _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.h!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h2!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h3!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h4!1: {{.*}} : _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.i!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i2!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i3!1: {{.*}} : hidden _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i4!1: {{.*}} : _T013vtable_thunks1F{{[A-Z0-9a-z_]*}}F

// CHECK-LABEL: sil_vtable NoThrowVariance {
// CHECK:         #ThrowVariance.mightThrow!1: {{.*}} : _T013vtable_thunks{{[A-Z0-9a-z_]*}}F

