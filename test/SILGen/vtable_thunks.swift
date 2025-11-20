// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s

protocol AddrOnly {}

func callMethodsOnD<U>(d: D, b: B, a: AddrOnly, u: U, i: Int) {
  _ = d.iuo(x: b, y: b, z: b)
  _ = d.f(x: b, y: b)
  _ = d.f2(x: b, y: b)
  _ = d.f3(x: b, y: b)
  _ = d.f4(x: b, y: b)
  _ = d.g(x: a, y: a)
  _ = d.g2(x: a, y: a)
  _ = d.g3(x: a, y: a)
  _ = d.g4(x: a, y: a)
  _ = d.h(x: u, y: u)
  _ = d.h2(x: u, y: u)
  _ = d.h3(x: u, y: u)
  _ = d.h4(x: u, y: u)
  _ = d.i(x: i, y: i)
  _ = d.i2(x: i, y: i)
  _ = d.i3(x: i, y: i)
  _ = d.i4(x: i, y: i)
}

func callMethodsOnF<U>(d: F, b: B, a: AddrOnly, u: U, i: Int) {
  _ = d.iuo(x: b, y: b, z: b)
  _ = d.f(x: b, y: b)
  _ = d.f2(x: b, y: b)
  _ = d.f3(x: b, y: b)
  _ = d.f4(x: b, y: b)
  _ = d.g(x: a, y: a)
  _ = d.g2(x: a, y: a)
  _ = d.g3(x: a, y: a)
  _ = d.g4(x: a, y: a)
  _ = d.h(x: u, y: u)
  _ = d.h2(x: u, y: u)
  _ = d.h3(x: u, y: u)
  _ = d.h4(x: u, y: u)
  _ = d.i(x: i, y: i)
  _ = d.i2(x: i, y: i)
  _ = d.i3(x: i, y: i)
  _ = d.i4(x: i, y: i)
}

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

protocol Proto {}

class G {
  func foo<T: Proto>(arg: T) {}
}

class H: G {
  override func foo<T>(arg: T) {}
}

// This test is incorrect in semantic SIL today. But it will be fixed in
// forthcoming commits.
//
// CHECK-LABEL: sil private [thunk] [ossa] @$s13vtable_thunks1DC3iuo{{[_0-9a-zA-Z]*}}FTV
// CHECK: bb0([[X:%.*]] : @guaranteed $B, [[Y:%.*]] : @guaranteed $Optional<B>, [[Z:%.*]] : @guaranteed $B, [[W:%.*]] : @guaranteed $D):
// CHECK:   [[WRAP_X:%.*]] = enum $Optional<B>, #Optional.some!enumelt, [[X]] : $B
// CHECK:   switch_enum [[Y]] : $Optional<B>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]

// CHECK: [[NONE_BB]]:
// CHECK:   [[DIAGNOSE_UNREACHABLE_FUNC:%.*]] = function_ref @$ss30_diagnoseUnexpectedNilOptional{{.*}}
// CHECK:   apply [[DIAGNOSE_UNREACHABLE_FUNC]]
// CHECK:   unreachable

// CHECK: [[SOME_BB]]([[UNWRAP_Y:%.*]] : @guaranteed $B):
// CHECK:   [[THUNK_FUNC:%.*]] = function_ref @$s13vtable_thunks1DC3iuo{{.*}}
// CHECK:   [[RES:%.*]] = apply [[THUNK_FUNC]]([[WRAP_X]], [[UNWRAP_Y]], [[Z]], [[W]])
// CHECK:   [[WRAP_RES:%.*]] = enum $Optional<B>, {{.*}} [[RES]]
// CHECK:   return [[WRAP_RES]]

// CHECK-LABEL: sil private [thunk] [ossa] @$s13vtable_thunks1DC1g{{[_0-9a-zA-Z]*}}FTV
// TODO: extra copies here
// CHECK:         [[WRAPPED_X_ADDR:%.*]] = init_enum_data_addr [[WRAP_X_ADDR:%.*]] :
// CHECK:         copy_addr [take] {{%.*}} to [init] [[WRAPPED_X_ADDR]]
// CHECK:         inject_enum_addr [[WRAP_X_ADDR]]
// CHECK:         [[RES_ADDR:%.*]] = alloc_stack
// CHECK:         apply {{%.*}}([[RES_ADDR]], [[WRAP_X_ADDR]], %2, %3)
// CHECK:         [[DEST_ADDR:%.*]] = init_enum_data_addr %0
// CHECK:         copy_addr [take] [[RES_ADDR]] to [init] [[DEST_ADDR]]
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

// rdar://problem/59669591
class Base {
  func returnsOptional() -> Int? { return nil }
}

class Derived<Foo> : Base {
  // The thunk here needs to be generic.
  override func returnsOptional() -> Int { return 0 }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s13vtable_thunks3BarC3foo{{[_0-9a-zA-Z]*}}FTV : $@convention(method) (@guaranteed @callee_guaranteed (Int) -> Int, @guaranteed Bar) -> @owned Optional<@callee_guaranteed (Int) -> Int>
// CHECK:         [[IMPL:%.*]] = function_ref @$s13vtable_thunks3BarC3foo{{[_0-9a-zA-Z]*}}F
// CHECK:         apply [[IMPL]]

// CHECK-LABEL: sil private [thunk] [ossa] @$s13vtable_thunks4NootC4flip{{[_0-9a-zA-Z]*}}FTV
// CHECK:         [[IMPL:%.*]] = function_ref @$s13vtable_thunks4NootC4flip{{[_0-9a-zA-Z]*}}F
// CHECK:         [[INNER:%.*]] = apply %1(%0)
// CHECK:         [[THUNK:%.*]] = function_ref @$s13vtable_thunks1SVIegd_ACSgIegd_TR
// CHECK:         [[OUTER:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[INNER]])
// CHECK:         return [[OUTER]]

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s13vtable_thunks1SVIegd_ACSgIegd_TR
// CHECK:         [[INNER:%.*]] = apply %0()
// CHECK:         [[OUTER:%.*]] = enum $Optional<S>, #Optional.some!enumelt, [[INNER]] : $S
// CHECK:         return [[OUTER]] : $Optional<S>

// CHECK-LABEL: sil private [thunk] [ossa] @$s13vtable_thunks4NootC3map{{[_0-9a-zA-Z]*}}FTV
// CHECK:         [[IMPL:%.*]] = function_ref @$s13vtable_thunks4NootC3map{{[_0-9a-zA-Z]*}}F
// CHECK:         [[INNER:%.*]] = apply %1(%0)
// CHECK:         [[THUNK:%.*]] = function_ref @$s13vtable_thunks1SVSgAA4NootCIego_Iegyo_AcA3AapCSgIego_Iegyo_TR
// CHECK:         [[OUTER:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[INNER]])
// CHECK:         return [[OUTER]]

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$s13vtable_thunks1SVSgAA4NootCIego_Iegyo_AcA3AapCSgIego_Iegyo_TR
// CHECK:         [[ARG:%.*]] = enum $Optional<S>, #Optional.some!enumelt, %0
// CHECK:         [[INNER:%.*]] = apply %1([[ARG]])
// CHECK:         [[OUTER:%.*]] = convert_function [[INNER]] : $@callee_guaranteed () -> @owned Noot to $@callee_guaranteed () -> @owned Optional<Aap>
// CHECK:         return [[OUTER]]

// CHECK-LABEL: sil private [thunk] [ossa] @$s13vtable_thunks7DerivedC15returnsOptionalSiyFAA4BaseCADSiSgyFTV : $@convention(method) <τ_0_0> (@guaranteed Derived<τ_0_0>) -> Optional<Int> {

// CHECK-LABEL: sil_vtable D {
// CHECK:         #B.iuo: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.f: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f2: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f3: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f4: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.g: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g2: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g3: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g4: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.h: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h2: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h3: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h4: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.i: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i2: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i3: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i4: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F

// CHECK-LABEL: sil_vtable E {
// CHECK:         #B.iuo: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.f: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f2: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f3: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f4: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.g: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g2: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g3: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g4: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.h: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h2: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h3: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h4: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.i: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i2: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i3: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i4: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}F

// CHECK-LABEL: sil_vtable F {
// CHECK:         #B.iuo: {{.*}} : @$s13vtable_thunks1D{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.f: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f2: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f3: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.f4: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.g: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g2: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g3: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.g4: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.h: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h2: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h3: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.h4: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}F
// CHECK:         #B.i: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i2: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i3: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}FTV
// CHECK:         #B.i4: {{.*}} : @$s13vtable_thunks1F{{[A-Z0-9a-z_]*}}F

// CHECK-LABEL: sil_vtable H {
// CHECK:  #G.foo: <T where T : Proto> (G) -> (T) -> () : @$s13vtable_thunks1H{{[A-Z0-9a-z_]*}}FTV [override]
// CHECK:  #H.foo: <T> (H) -> (T) -> () : @$s13vtable_thunks1H{{[A-Z0-9a-z_]*}}tlF

// CHECK-LABEL: sil_vtable NoThrowVariance {
// CHECK:         #ThrowVariance.mightThrow: {{.*}} : @$s13vtable_thunks{{[A-Z0-9a-z_]*}}F

// CHECK-LABEL: sil_vtable Base {
// CHECK:         #Base.returnsOptional: (Base) -> () -> Int? : @$s13vtable_thunks4BaseC15returnsOptionalSiSgyF

// CHECK-LABEL: sil_vtable Derived {
// CHECK:         #Base.returnsOptional: (Base) -> () -> Int? : @$s13vtable_thunks7DerivedC15returnsOptionalSiyFAA4BaseCADSiSgyFTV [override]
