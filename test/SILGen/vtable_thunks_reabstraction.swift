// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

struct S {}
class B {}
class C: B {}
class D: C {}

func callMethodsOnOpaque<T, U>(o: Opaque<T>, t: T, u: U, tt: T.Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnStillOpaque<T, U>(o: StillOpaque<T>, t: T, u: U, tt: T.Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteValue<U>(o: ConcreteValue, t: S, u: U, tt: S.Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteClass<U>(o: ConcreteClass, t: C, u: U, tt: C.Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteClassVariance<U>(o: ConcreteClassVariance, b: B, c: C, u: U, tt: C.Type) {
  _ = o.inAndOut(x: b)
  _ = o.inAndOutGeneric(x: c, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (c, (tt, { $0 })))
  _ = o.variantOptionality(x: b)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (c, (tt, { $0 })))
}

func callMethodsOnOpaqueTuple<T, U>(o: OpaqueTuple<T>, t: (T, T), u: U, tt: (T, T).Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteTuple<U>(o: ConcreteTuple, t: (S, S), u: U, tt: (S, S).Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnOpaqueFunction<X, Y, U>(o: OpaqueFunction<X, Y>, t: @escaping (X) -> Y, u: U, tt: ((X) -> Y).Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteFunction<U>(o: ConcreteFunction, t: @escaping (S) -> S, u: U, tt: ((S) -> S).Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnOpaqueMetatype<T, U>(o: OpaqueMetatype<T>, t: T.Type, u: U, tt: T.Type.Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteValueMetatype<U>(o: ConcreteValueMetatype, t: S.Type, u: U, tt: S.Type.Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteClassMetatype<U>(o: ConcreteClassMetatype, t: C.Type, u: U, tt: C.Type.Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteOptional<U>(o: ConcreteOptional, t: S?, u: U, tt: S?.Type) {
  _ = o.inAndOut(x: t)
  _ = o.inAndOutGeneric(x: t, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (t, (tt, { $0 })))
  _ = o.variantOptionality(x: t)
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

class Opaque<T> {
  typealias ObnoxiousTuple = (T, (T.Type, (T) -> T))

  func inAndOut(x: T) -> T { return x }
  func inAndOutGeneric<U>(x: T, y: U) -> U { return y }
  func inAndOutMetatypes(x: T.Type) -> T.Type { return x }
  func inAndOutFunctions(x: @escaping (T) -> T) -> (T) -> T { return x }
  func inAndOutTuples(x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  func variantOptionality(x: T) -> T? { return x }
  func variantOptionalityMetatypes(x: T.Type) -> T.Type? { return x }
  func variantOptionalityFunctions(x: @escaping (T) -> T) -> ((T) -> T)? { return x }
  func variantOptionalityTuples(x: ObnoxiousTuple) -> ObnoxiousTuple? { return x }
}

// CHECK-LABEL: sil_vtable Opaque {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC8inAndOut1xxx_tF    // Opaque.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF     // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF     // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF     // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : @$s27vtable_thunks_reabstraction6OpaqueC18variantOptionality1xxSgx_tF    // Opaque.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF      // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF    // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF  // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction6OpaqueCACyxGycfC
// CHECK-NEXT:   #Opaque.deinit!deallocator.1: @$s27vtable_thunks_reabstraction6OpaqueCfD        // Opaque.__deallocating_deinit
// CHECK-NEXT: }

class StillOpaque<T>: Opaque<T> {
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

// CHECK-LABEL: sil_vtable StillOpaque {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC8inAndOut1xxx_tF [inherited]        // Opaque.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : @$s27vtable_thunks_reabstraction6OpaqueC18variantOptionality1xxSgx_tF [inherited]        // Opaque.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : hidden @$s27vtable_thunks_reabstraction11StillOpaqueC24variantOptionalityTuples1xx_xm_xxcttx_xm_xxcttSg_tFAA0E0CAdeFx_xm_xxctt_tFTV [override] // vtable thunk for Opaque.variantOptionalityTuples(x:) dispatching to StillOpaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction11StillOpaqueCACyxGycfC [override]

// Tuple becomes more optional -- needs new vtable entry

// CHECK-NEXT:   #StillOpaque.variantOptionalityTuples!1: <T> (StillOpaque<T>) -> ((T, (T.Type, (T) -> T))?) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction11StillOpaqueC24variantOptionalityTuples1xx_xm_xxcttx_xm_xxcttSg_tF  // StillOpaque.variantOptionalityTuples(x:)

// CHECK-NEXT:   #StillOpaque.deinit!deallocator.1: @$s27vtable_thunks_reabstraction11StillOpaqueCfD     // StillOpaque.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteValue: Opaque<S> {
  override func inAndOut(x: S) -> S { return x }
  override func inAndOutGeneric<Z>(x: S, y: Z) -> Z { return y }
  override func inAndOutMetatypes(x: S.Type) -> S.Type { return x }
  override func inAndOutFunctions(x: @escaping (S) -> S) -> (S) -> S { return x }
  override func inAndOutTuples(x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  override func variantOptionality(x: S?) -> S { return x! }
  override func variantOptionalityMetatypes(x: S.Type?) -> S.Type { return x! }
  override func variantOptionalityFunctions(x: ((S) -> S)?) -> (S) -> S { return x! }
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteValue {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC8inAndOut1xAA1SVAG_tFAA6OpaqueCAdExx_tFTV [override] // vtable thunk for Opaque.inAndOut(x:) dispatching to ConcreteValue.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC15inAndOutGeneric1x1yxAA1SV_xtlFAA6OpaqueCAdeFqd__x_qd__tlFTV [override]        // vtable thunk for Opaque.inAndOutGeneric<A>(x:y:) dispatching to ConcreteValue.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC17inAndOutMetatypes1xAA1SVmAGm_tFAA6OpaqueCAdExmxm_tFTV [override]        // vtable thunk for Opaque.inAndOutMetatypes(x:) dispatching to ConcreteValue.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC17inAndOutFunctions1xAA1SVAGcA2Gc_tFAA6OpaqueCAdExxcxxc_tFTV [override]     // vtable thunk for Opaque.inAndOutFunctions(x:) dispatching to ConcreteValue.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC14inAndOutTuples1xAA1SV_AGm_A2GcttAG_AGm_A2Gctt_tFAA6OpaqueCAdEx_xm_xxcttx_xm_xxctt_tFTV [override]        // vtable thunk for Opaque.inAndOutTuples(x:) dispatching to ConcreteValue.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC18variantOptionality1xAA1SVAGSg_tFAA6OpaqueCAdExSgx_tFTV [override]       // vtable thunk for Opaque.variantOptionality(x:) dispatching to ConcreteValue.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC27variantOptionalityMetatypes1xAA1SVmAGmSg_tFAA6OpaqueCAdExmSgxm_tFTV [override]       // vtable thunk for Opaque.variantOptionalityMetatypes(x:) dispatching to ConcreteValue.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC27variantOptionalityFunctions1xAA1SVAGcA2GcSg_tFAA6OpaqueCAdExxcSgxxc_tFTV [override]  // vtable thunk for Opaque.variantOptionalityFunctions(x:) dispatching to ConcreteValue.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : hidden @$s27vtable_thunks_reabstraction13ConcreteValueC24variantOptionalityTuples1xAA1SV_AGm_A2GcttAG_AGm_A2GcttSg_tFAA6OpaqueCAdEx_xm_xxcttSgx_xm_xxctt_tFTV [override]       // vtable thunk for Opaque.variantOptionalityTuples(x:) dispatching to ConcreteValue.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction13ConcreteValueCACycfC [override]

// Value types becoming more optional -- needs new vtable entry

// CHECK-NEXT:   #ConcreteValue.variantOptionality!1: (ConcreteValue) -> (S?) -> S : @$s27vtable_thunks_reabstraction13ConcreteValueC18variantOptionality1xAA1SVAGSg_tF        // ConcreteValue.variantOptionality(x:)
// CHECK-NEXT:   #ConcreteValue.variantOptionalityMetatypes!1: (ConcreteValue) -> (S.Type?) -> S.Type : @$s27vtable_thunks_reabstraction13ConcreteValueC27variantOptionalityMetatypes1xAA1SVmAGmSg_tF  // ConcreteValue.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #ConcreteValue.variantOptionalityFunctions!1: (ConcreteValue) -> (((S) -> S)?) -> (S) -> S : @$s27vtable_thunks_reabstraction13ConcreteValueC27variantOptionalityFunctions1xAA1SVAGcA2GcSg_tF // ConcreteValue.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #ConcreteValue.variantOptionalityTuples!1: (ConcreteValue) -> ((S, (S.Type, (S) -> S))?) -> (S, (S.Type, (S) -> S)) : @$s27vtable_thunks_reabstraction13ConcreteValueC24variantOptionalityTuples1xAA1SV_AGm_A2GcttAG_AGm_A2GcttSg_tF  // ConcreteValue.variantOptionalityTuples(x:)

// CHECK-NEXT:   #ConcreteValue.deinit!deallocator.1: @$s27vtable_thunks_reabstraction13ConcreteValueCfD // ConcreteValue.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteClass: Opaque<C> {
  override func inAndOut(x: C) -> C { return x }
  override func inAndOutMetatypes(x: C.Type) -> C.Type { return x }
  override func inAndOutFunctions(x: @escaping (C) -> C) -> (C) -> C { return x }
  override func inAndOutTuples(x: ObnoxiousTuple) -> ObnoxiousTuple { return x }
  override func variantOptionality(x: C?) -> C { return x! }
  override func variantOptionalityMetatypes(x: C.Type?) -> C.Type { return x! }
  override func variantOptionalityFunctions(x: ((C) -> C)?) -> (C) -> C { return x! }
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteClass {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction13ConcreteClassC8inAndOut1xAA1CCAG_tFAA6OpaqueCAdExx_tFTV [override] // vtable thunk for Opaque.inAndOut(x:) dispatching to ConcreteClass.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction13ConcreteClassC17inAndOutMetatypes1xAA1CCmAGm_tF [override]     // ConcreteClass.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction13ConcreteClassC17inAndOutFunctions1xAA1CCAGcA2Gc_tFAA6OpaqueCAdExxcxxc_tFTV [override]     // vtable thunk for Opaque.inAndOutFunctions(x:) dispatching to ConcreteClass.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : hidden @$s27vtable_thunks_reabstraction13ConcreteClassC14inAndOutTuples1xAA1CC_AGm_A2GcttAG_AGm_A2Gctt_tFAA6OpaqueCAdEx_xm_xxcttx_xm_xxctt_tFTV [override]        // vtable thunk for Opaque.inAndOutTuples(x:) dispatching to ConcreteClass.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction13ConcreteClassC18variantOptionality1xAA1CCAGSg_tFAA6OpaqueCAdExSgx_tFTV [override]       // vtable thunk for Opaque.variantOptionality(x:) dispatching to ConcreteClass.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction13ConcreteClassC27variantOptionalityMetatypes1xAA1CCmAGmSg_tF [override]      // ConcreteClass.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : hidden @$s27vtable_thunks_reabstraction13ConcreteClassC27variantOptionalityFunctions1xAA1CCAGcA2GcSg_tFAA6OpaqueCAdExxcSgxxc_tFTV [override]  // vtable thunk for Opaque.variantOptionalityFunctions(x:) dispatching to ConcreteClass.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : hidden @$s27vtable_thunks_reabstraction13ConcreteClassC24variantOptionalityTuples1xAA1CC_AGm_A2GcttAG_AGm_A2GcttSg_tFAA6OpaqueCAdEx_xm_xxcttSgx_xm_xxctt_tFTV [override]       // vtable thunk for Opaque.variantOptionalityTuples(x:) dispatching to ConcreteClass.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction13ConcreteClassCACycfC [override]

// Class references are ABI-compatible with optional class references, and
// similarly for class metatypes.
//
// Function and tuple optionality change still needs a new vtable entry
// as above.

// CHECK-NEXT:   #ConcreteClass.variantOptionalityFunctions!1: (ConcreteClass) -> (((C) -> C)?) -> (C) -> C : @$s27vtable_thunks_reabstraction13ConcreteClassC27variantOptionalityFunctions1xAA1CCAGcA2GcSg_tF // ConcreteClass.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #ConcreteClass.variantOptionalityTuples!1: (ConcreteClass) -> ((C, (C.Type, (C) -> C))?) -> (C, (C.Type, (C) -> C)) : @$s27vtable_thunks_reabstraction13ConcreteClassC24variantOptionalityTuples1xAA1CC_AGm_A2GcttAG_AGm_A2GcttSg_tF  // ConcreteClass.variantOptionalityTuples(x:)

// CHECK-NEXT:   #ConcreteClass.deinit!deallocator.1: @$s27vtable_thunks_reabstraction13ConcreteClassCfD // ConcreteClass.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteClassVariance: Opaque<C> {
  override func inAndOut(x: B) -> D { return x as! D }
  override func variantOptionality(x: B?) -> D { return x as! D }
}

// CHECK-LABEL: sil_vtable ConcreteClassVariance {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction21ConcreteClassVarianceC8inAndOut1xAA1DCAA1BC_tFAA6OpaqueCAdExx_tFTV [override]      // vtable thunk for Opaque.inAndOut(x:) dispatching to ConcreteClassVariance.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction21ConcreteClassVarianceC18variantOptionality1xAA1DCAA1BCSg_tFAA6OpaqueCAdExSgx_tFTV [override]    // vtable thunk for Opaque.variantOptionality(x:) dispatching to ConcreteClassVariance.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction21ConcreteClassVarianceCACycfC [override]

// No new vtable entries -- class references are ABI compatible with
// optional class references.

// CHECK-NEXT:   #ConcreteClassVariance.deinit!deallocator.1: @$s27vtable_thunks_reabstraction21ConcreteClassVarianceCfD // ConcreteClassVariance.__deallocating_deinit
// CHECK-NEXT: }

class OpaqueTuple<U>: Opaque<(U, U)> {
  override func inAndOut(x: (U, U)) -> (U, U) { return x }
  override func variantOptionality(x: (U, U)?) -> (U, U) { return x! }
}

// CHECK-LABEL: sil_vtable OpaqueTuple {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction11OpaqueTupleC8inAndOut1xx_xtx_xt_tFAA0D0CAdExx_tFTV [override]      // vtable thunk for Opaque.inAndOut(x:) dispatching to OpaqueTuple.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction11OpaqueTupleC18variantOptionality1xx_xtx_xtSg_tFAA0D0CAdExSgx_tFTV [override]    // vtable thunk for Opaque.variantOptionality(x:) dispatching to OpaqueTuple.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction11OpaqueTupleCACyxGycfC [override]

// Optionality change of tuple.

// CHECK-NEXT:   #OpaqueTuple.variantOptionality!1: <U> (OpaqueTuple<U>) -> ((U, U)?) -> (U, U) : @$s27vtable_thunks_reabstraction11OpaqueTupleC18variantOptionality1xx_xtx_xtSg_tF    // OpaqueTuple.variantOptionality(x:)

// CHECK-NEXT:   #OpaqueTuple.deinit!deallocator.1: @$s27vtable_thunks_reabstraction11OpaqueTupleCfD     // OpaqueTuple.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteTuple: Opaque<(S, S)> {
  override func inAndOut(x: (S, S)) -> (S, S) { return x }
  override func variantOptionality(x: (S, S)?) -> (S, S) { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteTuple {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction13ConcreteTupleC8inAndOut1xAA1SV_AGtAG_AGt_tFAA6OpaqueCAdExx_tFTV [override] // vtable thunk for Opaque.inAndOut(x:) dispatching to ConcreteTuple.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction13ConcreteTupleC18variantOptionality1xAA1SV_AGtAG_AGtSg_tFAA6OpaqueCAdExSgx_tFTV [override]       // vtable thunk for Opaque.variantOptionality(x:) dispatching to ConcreteTuple.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction13ConcreteTupleCACycfC [override]

// Optionality change of tuple.

// CHECK-NEXT:   #ConcreteTuple.variantOptionality!1: (ConcreteTuple) -> ((S, S)?) -> (S, S) : @$s27vtable_thunks_reabstraction13ConcreteTupleC18variantOptionality1xAA1SV_AGtAG_AGtSg_tF      // ConcreteTuple.variantOptionality(x:)

// CHECK-NEXT:   #ConcreteTuple.deinit!deallocator.1: @$s27vtable_thunks_reabstraction13ConcreteTupleCfD // ConcreteTuple.__deallocating_deinit
// CHECK-NEXT: }

class OpaqueFunction<U, V>: Opaque<(U) -> V> {
  override func inAndOut(x: @escaping (U) -> V) -> (U) -> V { return x }
  override func variantOptionality(x: ((U) -> V)?) -> (U) -> V { return x! }
}

// CHECK-LABEL: sil_vtable OpaqueFunction {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction14OpaqueFunctionC8inAndOut1xq_xcq_xc_tFAA0D0CAdExx_tFTV [override]   // vtable thunk for Opaque.inAndOut(x:) dispatching to OpaqueFunction.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction14OpaqueFunctionC18variantOptionality1xq_xcq_xcSg_tFAA0D0CAdExSgx_tFTV [override] // vtable thunk for Opaque.variantOptionality(x:) dispatching to OpaqueFunction.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction14OpaqueFunctionCACyxq_GycfC [override]

// Optionality change of function.

// CHECK-NEXT:   #OpaqueFunction.variantOptionality!1: <U, V> (OpaqueFunction<U, V>) -> (((U) -> V)?) -> (U) -> V : @$s27vtable_thunks_reabstraction14OpaqueFunctionC18variantOptionality1xq_xcq_xcSg_tF       // OpaqueFunction.variantOptionality(x:)

// CHECK-NEXT:   #OpaqueFunction.deinit!deallocator.1: @$s27vtable_thunks_reabstraction14OpaqueFunctionCfD       // OpaqueFunction.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteFunction: Opaque<(S) -> S> {
  override func inAndOut(x: @escaping (S) -> S) -> (S) -> S { return x }
  override func variantOptionality(x: ((S) -> S)?) -> (S) -> S { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteFunction {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction16ConcreteFunctionC8inAndOut1xAA1SVAGcA2Gc_tFAA6OpaqueCAdExx_tFTV [override] // vtable thunk for Opaque.inAndOut(x:) dispatching to ConcreteFunction.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction16ConcreteFunctionC18variantOptionality1xAA1SVAGcA2GcSg_tFAA6OpaqueCAdExSgx_tFTV [override]       // vtable thunk for Opaque.variantOptionality(x:) dispatching to ConcreteFunction.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction16ConcreteFunctionCACycfC [override]

// Optionality change of function.

// CHECK-NEXT:   #ConcreteFunction.variantOptionality!1: (ConcreteFunction) -> (((S) -> S)?) -> (S) -> S : @$s27vtable_thunks_reabstraction16ConcreteFunctionC18variantOptionality1xAA1SVAGcA2GcSg_tF  // ConcreteFunction.variantOptionality(x:)

// CHECK-NEXT:   #ConcreteFunction.deinit!deallocator.1: @$s27vtable_thunks_reabstraction16ConcreteFunctionCfD   // ConcreteFunction.__deallocating_deinit
// CHECK-NEXT: }

class OpaqueMetatype<U>: Opaque<U.Type> {
  override func inAndOut(x: U.Type) -> U.Type { return x }
  override func variantOptionality(x: U.Type?) -> U.Type { return x! }
}

// CHECK-LABEL: sil_vtable OpaqueMetatype {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction14OpaqueMetatypeC8inAndOut1xxmxm_tFAA0D0CAdExx_tFTV [override]       // vtable thunk for Opaque.inAndOut(x:) dispatching to OpaqueMetatype.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction14OpaqueMetatypeC18variantOptionality1xxmxmSg_tFAA0D0CAdExSgx_tFTV [override]     // vtable thunk for Opaque.variantOptionality(x:) dispatching to OpaqueMetatype.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction14OpaqueMetatypeCACyxGycfC [override]

// Optionality change of metatype.

// CHECK-NEXT:   #OpaqueMetatype.variantOptionality!1: <U> (OpaqueMetatype<U>) -> (U.Type?) -> U.Type : @$s27vtable_thunks_reabstraction14OpaqueMetatypeC18variantOptionality1xxmxmSg_tF       // OpaqueMetatype.variantOptionality(x:)

// CHECK-NEXT:   #OpaqueMetatype.deinit!deallocator.1: @$s27vtable_thunks_reabstraction14OpaqueMetatypeCfD       // OpaqueMetatype.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteValueMetatype: Opaque<S.Type> {
  override func inAndOut(x: S.Type) -> S.Type { return x }
  override func variantOptionality(x: S.Type?) -> S.Type { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteValueMetatype {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction21ConcreteValueMetatypeC8inAndOut1xAA1SVmAGm_tFAA6OpaqueCAdExx_tFTV [override]       // vtable thunk for Opaque.inAndOut(x:) dispatching to ConcreteValueMetatype.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction21ConcreteValueMetatypeC18variantOptionality1xAA1SVmAGmSg_tFAA6OpaqueCAdExSgx_tFTV [override]     // vtable thunk for Opaque.variantOptionality(x:) dispatching to ConcreteValueMetatype.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction21ConcreteValueMetatypeCACycfC [override]

// Optionality change of metatype.

// CHECK-NEXT:   #ConcreteValueMetatype.variantOptionality!1: (ConcreteValueMetatype) -> (S.Type?) -> S.Type : @$s27vtable_thunks_reabstraction21ConcreteValueMetatypeC18variantOptionality1xAA1SVmAGmSg_tF    // ConcreteValueMetatype.variantOptionality(x:)

// CHECK-NEXT:   #ConcreteValueMetatype.deinit!deallocator.1: @$s27vtable_thunks_reabstraction21ConcreteValueMetatypeCfD // ConcreteValueMetatype.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteClassMetatype: Opaque<C.Type> {
  override func inAndOut(x: C.Type) -> C.Type { return x }
  override func variantOptionality(x: C.Type?) -> C.Type { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteClassMetatype {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction21ConcreteClassMetatypeC8inAndOut1xAA1CCmAGm_tFAA6OpaqueCAdExx_tFTV [override]       // vtable thunk for Opaque.inAndOut(x:) dispatching to ConcreteClassMetatype.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden @$s27vtable_thunks_reabstraction21ConcreteClassMetatypeC18variantOptionality1xAA1CCmAGmSg_tFAA6OpaqueCAdExSgx_tFTV [override]     // vtable thunk for Opaque.variantOptionality(x:) dispatching to ConcreteClassMetatype.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction21ConcreteClassMetatypeCACycfC [override]

// Class metatypes are ABI compatible with optional class metatypes.

// CHECK-NEXT:   #ConcreteClassMetatype.deinit!deallocator.1: @$s27vtable_thunks_reabstraction21ConcreteClassMetatypeCfD // ConcreteClassMetatype.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteOptional: Opaque<S?> {
  override func inAndOut(x: S?) -> S? { return x }

  // FIXME: Should we allow this override in Sema?
  // override func variantOptionality(x: S??) -> S? { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteOptional {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden @$s27vtable_thunks_reabstraction16ConcreteOptionalC8inAndOut1xAA1SVSgAH_tFAA6OpaqueCAdExx_tFTV [override]    // vtable thunk for Opaque.inAndOut(x:) dispatching to ConcreteOptional.inAndOut(x:)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : @$s27vtable_thunks_reabstraction6OpaqueC15inAndOutGeneric1x1yqd__x_qd__tlF [inherited] // Opaque.inAndOutGeneric<A>(x:y:)
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypes1xxmxm_tF [inherited] // Opaque.inAndOutMetatypes(x:)
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : @$s27vtable_thunks_reabstraction6OpaqueC17inAndOutFunctions1xxxcxxc_tF [inherited] // Opaque.inAndOutFunctions(x:)
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : @$s27vtable_thunks_reabstraction6OpaqueC14inAndOutTuples1xx_xm_xxcttx_xm_xxctt_tF [inherited]     // Opaque.inAndOutTuples(x:)
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : @$s27vtable_thunks_reabstraction6OpaqueC18variantOptionality1xxSgx_tF [inherited]        // Opaque.variantOptionality(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypes1xxmSgxm_tF [inherited]  // Opaque.variantOptionalityMetatypes(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : @$s27vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctions1xxxcSgxxc_tF [inherited]        // Opaque.variantOptionalityFunctions(x:)
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : @$s27vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuples1xx_xm_xxcttSgx_xm_xxctt_tF [inherited]      // Opaque.variantOptionalityTuples(x:)
// CHECK-NEXT:   #Opaque.init!allocator.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : @$s27vtable_thunks_reabstraction16ConcreteOptionalCACycfC [override]
// CHECK-NEXT:   #ConcreteOptional.deinit!deallocator.1: @$s27vtable_thunks_reabstraction16ConcreteOptionalCfD   // ConcreteOptional.__deallocating_deinit
// CHECK-NEXT: }

// Make sure we remap the method's innermost generic parameters
// to the correct depth
class GenericBase<T> {
  func doStuff<U>(t: T, u: U) {}
  init<U>(t: T, u: U) {}
}

// CHECK-LABEL: sil_vtable GenericBase {
// CHECK-NEXT:   #GenericBase.doStuff!1: <T><U> (GenericBase<T>) -> (T, U) -> () : @$s27vtable_thunks_reabstraction11GenericBaseC7doStuff1t1uyx_qd__tlF        // GenericBase.doStuff<A>(t:u:)
// CHECK-NEXT:   #GenericBase.init!allocator.1: <T><U> (GenericBase<T>.Type) -> (T, U) -> GenericBase<T> : @$s27vtable_thunks_reabstraction11GenericBaseC1t1uACyxGx_qd__tclufC
// CHECK-NEXT:   #GenericBase.deinit!deallocator.1: @$s27vtable_thunks_reabstraction11GenericBaseCfD     // GenericBase.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteSub : GenericBase<Int> {
  override func doStuff<U>(t: Int, u: U) {
    super.doStuff(t: t, u: u)
  }
  override init<U>(t: Int, u: U) {
    super.init(t: t, u: u)
  }
}

// CHECK-LABEL: sil_vtable ConcreteSub {
// CHECK-NEXT:   #GenericBase.doStuff!1: <T><U> (GenericBase<T>) -> (T, U) -> () : hidden @$s27vtable_thunks_reabstraction11ConcreteSubC7doStuff1t1uySi_xtlFAA11GenericBaseCAdeFyx_qd__tlFTV [override]        // vtable thunk for GenericBase.doStuff<A>(t:u:) dispatching to ConcreteSub.doStuff<A>(t:u:)
// CHECK-NEXT:   #GenericBase.init!allocator.1: <T><U> (GenericBase<T>.Type) -> (T, U) -> GenericBase<T> : hidden @$s27vtable_thunks_reabstraction11ConcreteSubC1t1uACSi_xtclufCAA11GenericBaseCAdeGyxGx_qd__tclufCTV [override]
// CHECK-NEXT:   #ConcreteSub.deinit!deallocator.1: @$s27vtable_thunks_reabstraction11ConcreteSubCfD     // ConcreteSub.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteBase {
  init<U>(t: Int, u: U) {}
  func doStuff<U>(t: Int, u: U) {}
}

// CHECK-LABEL: sil_vtable ConcreteBase {
// CHECK-NEXT:   #ConcreteBase.init!allocator.1: <U> (ConcreteBase.Type) -> (Int, U) -> ConcreteBase : @$s27vtable_thunks_reabstraction12ConcreteBaseC1t1uACSi_xtclufC
// CHECK-NEXT:   #ConcreteBase.doStuff!1: <U> (ConcreteBase) -> (Int, U) -> () : @$s27vtable_thunks_reabstraction12ConcreteBaseC7doStuff1t1uySi_xtlF   // ConcreteBase.doStuff<A>(t:u:)
// CHECK-NEXT:   #ConcreteBase.deinit!deallocator.1: @$s27vtable_thunks_reabstraction12ConcreteBaseCfD   // ConcreteBase.__deallocating_deinit
// CHECK-NEXT: }

class GenericSub<T> : ConcreteBase {
  override init<U>(t: Int, u: U) {
    super.init(t: t, u: u)
  }
  override func doStuff<U>(t: Int, u: U) {
    super.doStuff(t: t, u: u)
  }
}

// CHECK-LABEL: sil_vtable GenericSub {
// CHECK-NEXT:   #ConcreteBase.init!allocator.1: <U> (ConcreteBase.Type) -> (Int, U) -> ConcreteBase : @$s27vtable_thunks_reabstraction10GenericSubC1t1uACyxGSi_qd__tclufC [override]
// CHECK-NEXT:   #ConcreteBase.doStuff!1: <U> (ConcreteBase) -> (Int, U) -> () : @$s27vtable_thunks_reabstraction10GenericSubC7doStuff1t1uySi_qd__tlF [override]       // GenericSub.doStuff<A>(t:u:)
// CHECK-NEXT:   #GenericSub.deinit!deallocator.1: @$s27vtable_thunks_reabstraction10GenericSubCfD       // GenericSub.__deallocating_deinit
// CHECK-NEXT: }

// Issue with generic parameter index
class MoreGenericSub1<T, TT> : GenericBase<T> {
  override func doStuff<U>(t: T, u: U) {
    super.doStuff(t: t, u: u)
  }
}

// CHECK-LABEL: sil_vtable MoreGenericSub1 {
// CHECK-NEXT:   #GenericBase.doStuff!1: <T><U> (GenericBase<T>) -> (T, U) -> () : @$s27vtable_thunks_reabstraction15MoreGenericSub1C7doStuff1t1uyx_qd__tlF [override] // MoreGenericSub1.doStuff<A>(t:u:)
// CHECK-NEXT:   #GenericBase.init!allocator.1: <T><U> (GenericBase<T>.Type) -> (T, U) -> GenericBase<T> : @$s27vtable_thunks_reabstraction15MoreGenericSub1C1t1uACyxq_Gx_qd__tclufC [override]
// CHECK-NEXT:   #MoreGenericSub1.deinit!deallocator.1: @$s27vtable_thunks_reabstraction15MoreGenericSub1CfD     // MoreGenericSub1.__deallocating_deinit
// CHECK-NEXT: }

class MoreGenericSub2<TT, T> : GenericBase<T> {
  override func doStuff<U>(t: T, u: U) {
    super.doStuff(t: t, u: u)
  }
}

// CHECK-LABEL: sil_vtable MoreGenericSub2 {
// CHECK-NEXT:   #GenericBase.doStuff!1: <T><U> (GenericBase<T>) -> (T, U) -> () : @$s27vtable_thunks_reabstraction15MoreGenericSub2C7doStuff1t1uyq__qd__tlF [override]        // MoreGenericSub2.doStuff<A>(t:u:)
// CHECK-NEXT:   #GenericBase.init!allocator.1: <T><U> (GenericBase<T>.Type) -> (T, U) -> GenericBase<T> : @$s27vtable_thunks_reabstraction15MoreGenericSub2C1t1uACyxq_Gq__qd__tclufC [override]
// CHECK-NEXT:   #MoreGenericSub2.deinit!deallocator.1: @$s27vtable_thunks_reabstraction15MoreGenericSub2CfD     // MoreGenericSub2.__deallocating_deinit
// CHECK-NEXT: }
