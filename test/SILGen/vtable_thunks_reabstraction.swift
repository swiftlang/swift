// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

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
  //_ = o.variantOptionality(x: t) // FIXME
  _ = o.variantOptionalityMetatypes(x: tt)
  _ = o.variantOptionalityFunctions(x: { $0 })
  _ = o.variantOptionalityTuples(x: (t, (tt, { $0 })))
}

func callMethodsOnConcreteClassVariance<U>(o: ConcreteClassVariance, b: B, c: C, u: U, tt: C.Type) {
  //_ = o.inAndOut(x: b) // FIXME
  _ = o.inAndOutGeneric(x: c, y: u)
  _ = o.inAndOutMetatypes(x: tt)
  _ = o.inAndOutTuples(x: (c, (tt, { $0 })))
  //_ = o.variantOptionality(x: t) // FIXME
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
  //_ = o.variantOptionality(x: t) // FIXME
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
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC8inAndOutxx1x_tF	// Opaque.inAndOut(x : A) -> A
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : _T027vtable_thunks_reabstraction6OpaqueC18variantOptionalityxSgx1x_tF	// Opaque.variantOptionality(x : A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction6OpaqueCACyxGycfc	// Opaque.init() -> Opaque<A>
// CHECK-NEXT:   #Opaque.deinit!deallocator: _T027vtable_thunks_reabstraction6OpaqueCfD	// Opaque.__deallocating_deinit
// CHECK-NEXT: }

class StillOpaque<T>: Opaque<T> {
  override func variantOptionalityTuples(x: ObnoxiousTuple?) -> ObnoxiousTuple { return x! }
}

// CHECK-LABEL: sil_vtable StillOpaque {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC8inAndOutxx1x_tF	// Opaque.inAndOut(x : A) -> A
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : _T027vtable_thunks_reabstraction6OpaqueC18variantOptionalityxSgx1x_tF	// Opaque.variantOptionality(x : A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : hidden _T027vtable_thunks_reabstraction11StillOpaqueC24variantOptionalityTuplesx_xm_xxcttx_xm_xxcttSg1x_tFAA0E0CAdEx_xm_xxcttAF_tFTV	// vtable thunk for Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))? dispatching to StillOpaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))?) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction11StillOpaqueCACyxGycfc	// StillOpaque.init() -> StillOpaque<A>

// Tuple becomes more optional -- needs new vtable entry

// CHECK-NEXT:  #StillOpaque.variantOptionalityTuples!1: <T> (StillOpaque<T>) -> ((T, (T.Type, (T) -> T))?) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction11StillOpaqueC24variantOptionalityTuplesx_xm_xxcttx_xm_xxcttSg1x_tF	// StillOpaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))?) -> (A, (A.Type, (A) -> A))

// CHECK-NEXT:   #StillOpaque.deinit!deallocator: _T027vtable_thunks_reabstraction11StillOpaqueCfD	// StillOpaque.__deallocating_deinit
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
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction13ConcreteValueC8inAndOutAA1SVAF1x_tFAA6OpaqueCADxxAG_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to ConcreteValue.inAndOut(x : S) -> S
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : hidden _T027vtable_thunks_reabstraction13ConcreteValueC15inAndOutGenericxAA1SV1x_x1ytlFAA6OpaqueCADqd__xAG_qd__AHtlFTV	// vtable thunk for Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1 dispatching to ConcreteValue.inAndOutGeneric<A> (x : S, y : A) -> A
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : hidden _T027vtable_thunks_reabstraction13ConcreteValueC17inAndOutMetatypesAA1SVmAFm1x_tFAA6OpaqueCADxmxmAG_tFTV	// vtable thunk for Opaque.inAndOutMetatypes(x : A.Type) -> A.Type dispatching to ConcreteValue.inAndOutMetatypes(x : S.Type) -> S.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : hidden _T027vtable_thunks_reabstraction13ConcreteValueC17inAndOutFunctionsAA1SVAFcA2Fc1x_tFAA6OpaqueCADxxcxxcAG_tFTV	// vtable thunk for Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A dispatching to ConcreteValue.inAndOutFunctions(x : (S) -> S) -> (S) -> S
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : hidden _T027vtable_thunks_reabstraction13ConcreteValueC14inAndOutTuplesAA1SV_AFm_A2FcttAF_AFm_A2Fctt1x_tFAA6OpaqueCADx_xm_xxcttx_xm_xxcttAG_tFTV	// vtable thunk for Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A)) dispatching to ConcreteValue.inAndOutTuples(x : (S, (S.Type, (S) -> S))) -> (S, (S.Type, (S) -> S))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction13ConcreteValueC18variantOptionalityAA1SVAFSg1x_tFAA6OpaqueCADxSgxAH_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to ConcreteValue.variantOptionality(x : S?) -> S
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : hidden _T027vtable_thunks_reabstraction13ConcreteValueC27variantOptionalityMetatypesAA1SVmAFmSg1x_tFAA6OpaqueCADxmSgxmAH_tFTV	// vtable thunk for Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type? dispatching to ConcreteValue.variantOptionalityMetatypes(x : S.Type?) -> S.Type
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : hidden _T027vtable_thunks_reabstraction13ConcreteValueC27variantOptionalityFunctionsAA1SVAFcA2FcSg1x_tFAA6OpaqueCADxxcSgxxcAH_tFTV	// vtable thunk for Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A? dispatching to ConcreteValue.variantOptionalityFunctions(x : (S) -> S?) -> (S) -> S
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : hidden _T027vtable_thunks_reabstraction13ConcreteValueC24variantOptionalityTuplesAA1SV_AFm_A2FcttAF_AFm_A2FcttSg1x_tFAA6OpaqueCADx_xm_xxcttSgx_xm_xxcttAH_tFTV	// vtable thunk for Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))? dispatching to ConcreteValue.variantOptionalityTuples(x : (S, (S.Type, (S) -> S))?) -> (S, (S.Type, (S) -> S))
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction13ConcreteValueCACycfc	// ConcreteValue.init() -> ConcreteValue

// Value types becoming more optional -- needs new vtable entry

// CHECK-NEXT:   #ConcreteValue.variantOptionality!1: (ConcreteValue) -> (S?) -> S : _T027vtable_thunks_reabstraction13ConcreteValueC18variantOptionalityAA1SVAFSg1x_tF // ConcreteValue.variantOptionality(x : S?) -> S
// CHECK-NEXT:   #ConcreteValue.variantOptionalityMetatypes!1: (ConcreteValue) -> (S.Type?) -> S.Type : _T027vtable_thunks_reabstraction13ConcreteValueC27variantOptionalityMetatypesAA1SVmAFmSg1x_tF // ConcreteValue.variantOptionalityMetatypes(x : S.Type?) -> S.Type
// CHECK-NEXT:   #ConcreteValue.variantOptionalityFunctions!1: (ConcreteValue) -> (((S) -> S)?) -> (S) -> S : _T027vtable_thunks_reabstraction13ConcreteValueC27variantOptionalityFunctionsAA1SVAFcA2FcSg1x_tF	// ConcreteValue.variantOptionalityFunctions(x : (S) -> S?) -> (S) -> S
// CHECK-NEXT:   #ConcreteValue.variantOptionalityTuples!1: (ConcreteValue) -> ((S, (S.Type, (S) -> S))?) -> (S, (S.Type, (S) -> S)) : _T027vtable_thunks_reabstraction13ConcreteValueC24variantOptionalityTuplesAA1SV_AFm_A2FcttAF_AFm_A2FcttSg1x_tF	// ConcreteValue.variantOptionalityTuples(x : (S, (S.Type, (S) -> S))?) -> (S, (S.Type, (S) -> S))

// CHECK-NEXT:   #ConcreteValue.deinit!deallocator: _T027vtable_thunks_reabstraction13ConcreteValueCfD	// ConcreteValue.__deallocating_deinit
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
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction13ConcreteClassC8inAndOutAA1CCAF1x_tFAA6OpaqueCADxxAG_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to ConcreteClass.inAndOut(x : C) -> C
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction13ConcreteClassC17inAndOutMetatypesAA1CCmAFm1x_tF	// ConcreteClass.inAndOutMetatypes(x : C.Type) -> C.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : hidden _T027vtable_thunks_reabstraction13ConcreteClassC17inAndOutFunctionsAA1CCAFcA2Fc1x_tFAA6OpaqueCADxxcxxcAG_tFTV	// vtable thunk for Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A dispatching to ConcreteClass.inAndOutFunctions(x : (C) -> C) -> (C) -> C
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : hidden _T027vtable_thunks_reabstraction13ConcreteClassC14inAndOutTuplesAA1CC_AFm_A2FcttAF_AFm_A2Fctt1x_tFAA6OpaqueCADx_xm_xxcttx_xm_xxcttAG_tFTV	// vtable thunk for Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A)) dispatching to ConcreteClass.inAndOutTuples(x : (C, (C.Type, (C) -> C))) -> (C, (C.Type, (C) -> C))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction13ConcreteClassC18variantOptionalityAA1CCAFSg1x_tFAA6OpaqueCADxSgxAH_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to ConcreteClass.variantOptionality(x : C?) -> C
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction13ConcreteClassC27variantOptionalityMetatypesAA1CCmAFmSg1x_tF	// ConcreteClass.variantOptionalityMetatypes(x : C.Type?) -> C.Type
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : hidden _T027vtable_thunks_reabstraction13ConcreteClassC27variantOptionalityFunctionsAA1CCAFcA2FcSg1x_tFAA6OpaqueCADxxcSgxxcAH_tFTV	// vtable thunk for Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A? dispatching to ConcreteClass.variantOptionalityFunctions(x : (C) -> C?) -> (C) -> C
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : hidden _T027vtable_thunks_reabstraction13ConcreteClassC24variantOptionalityTuplesAA1CC_AFm_A2FcttAF_AFm_A2FcttSg1x_tFAA6OpaqueCADx_xm_xxcttSgx_xm_xxcttAH_tFTV	// vtable thunk for Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))? dispatching to ConcreteClass.variantOptionalityTuples(x : (C, (C.Type, (C) -> C))?) -> (C, (C.Type, (C) -> C))
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction13ConcreteClassCACycfc	// ConcreteClass.init() -> ConcreteClass

// Class references are ABI-compatible with optional class references, and
// similarly for class metatypes.
//
// Function and tuple optionality change still needs a new vtable entry
// as above.

// CHECK-NEXT:   #ConcreteClass.variantOptionalityFunctions!1: (ConcreteClass) -> (((C) -> C)?) -> (C) -> C : _T027vtable_thunks_reabstraction13ConcreteClassC27variantOptionalityFunctionsAA1CCAFcA2FcSg1x_tF	// ConcreteClass.variantOptionalityFunctions(x : (C) -> C?) -> (C) -> C
// CHECK-NEXT:   #ConcreteClass.variantOptionalityTuples!1: (ConcreteClass) -> ((C, (C.Type, (C) -> C))?) -> (C, (C.Type, (C) -> C)) : _T027vtable_thunks_reabstraction13ConcreteClassC24variantOptionalityTuplesAA1CC_AFm_A2FcttAF_AFm_A2FcttSg1x_tF	// ConcreteClass.variantOptionalityTuples(x : (C, (C.Type, (C) -> C))?) -> (C, (C.Type, (C) -> C))

// CHECK-NEXT:   #ConcreteClass.deinit!deallocator: _T027vtable_thunks_reabstraction13ConcreteClassCfD	// ConcreteClass.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteClassVariance: Opaque<C> {
  override func inAndOut(x: B) -> D { return x as! D }
  override func variantOptionality(x: B?) -> D { return x as! D }
}

// CHECK-LABEL: sil_vtable ConcreteClassVariance {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction21ConcreteClassVarianceC8inAndOutAA1DCAA1BC1x_tFAA6OpaqueCADxxAI_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to ConcreteClassVariance.inAndOut(x : B) -> D
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction21ConcreteClassVarianceC18variantOptionalityAA1DCAA1BCSg1x_tFAA6OpaqueCADxSgxAJ_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to ConcreteClassVariance.variantOptionality(x : B?) -> D
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction21ConcreteClassVarianceCACycfc	// ConcreteClassVariance.init() -> ConcreteClassVariance

// No new vtable entries -- class references are ABI compatible with
// optional class references.

// CHECK-NEXT:   #ConcreteClassVariance.deinit!deallocator: _T027vtable_thunks_reabstraction21ConcreteClassVarianceCfD	// ConcreteClassVariance.__deallocating_deinit
// CHECK-NEXT: }

class OpaqueTuple<U>: Opaque<(U, U)> {
  override func inAndOut(x: (U, U)) -> (U, U) { return x }
  override func variantOptionality(x: (U, U)?) -> (U, U) { return x! }
}

// CHECK-LABEL: sil_vtable OpaqueTuple {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction11OpaqueTupleC8inAndOutx_xtx_xt1x_tFAA0D0CADxxAE_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to OpaqueTuple.inAndOut(x : (A, A)) -> (A, A)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction11OpaqueTupleC18variantOptionalityx_xtx_xtSg1x_tFAA0D0CADxSgxAF_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to OpaqueTuple.variantOptionality(x : (A, A)?) -> (A, A)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction11OpaqueTupleCACyxGycfc	// OpaqueTuple.init() -> OpaqueTuple<A>

// Optionality change of tuple.

// CHECK-NEXT:  #OpaqueTuple.variantOptionality!1: <U> (OpaqueTuple<U>) -> ((U, U)?) -> (U, U) : _T027vtable_thunks_reabstraction11OpaqueTupleC18variantOptionalityx_xtx_xtSg1x_tF	// OpaqueTuple.variantOptionality(x : (A, A)?) -> (A, A)

// CHECK-NEXT:   #OpaqueTuple.deinit!deallocator: _T027vtable_thunks_reabstraction11OpaqueTupleCfD	// OpaqueTuple.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteTuple: Opaque<(S, S)> {
  override func inAndOut(x: (S, S)) -> (S, S) { return x }
  override func variantOptionality(x: (S, S)?) -> (S, S) { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteTuple {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction13ConcreteTupleC8inAndOutAA1SV_AFtAF_AFt1x_tFAA6OpaqueCADxxAG_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to ConcreteTuple.inAndOut(x : (S, S)) -> (S, S)
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction13ConcreteTupleC18variantOptionalityAA1SV_AFtAF_AFtSg1x_tFAA6OpaqueCADxSgxAH_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to ConcreteTuple.variantOptionality(x : (S, S)?) -> (S, S)
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction13ConcreteTupleCACycfc	// ConcreteTuple.init() -> ConcreteTuple

// Optionality change of tuple.

// CHECK-NEXT:   #ConcreteTuple.variantOptionality!1: (ConcreteTuple) -> ((S, S)?) -> (S, S) : _T027vtable_thunks_reabstraction13ConcreteTupleC18variantOptionalityAA1SV_AFtAF_AFtSg1x_tF	// ConcreteTuple.variantOptionality(x : (S, S)?) -> (S, S)

// CHECK-NEXT:   #ConcreteTuple.deinit!deallocator: _T027vtable_thunks_reabstraction13ConcreteTupleCfD	// ConcreteTuple.__deallocating_deinit
// CHECK-NEXT: }

class OpaqueFunction<U, V>: Opaque<(U) -> V> {
  override func inAndOut(x: @escaping (U) -> V) -> (U) -> V { return x }
  override func variantOptionality(x: ((U) -> V)?) -> (U) -> V { return x! }
}

// CHECK-LABEL: sil_vtable OpaqueFunction {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction14OpaqueFunctionC8inAndOutq_xcq_xc1x_tFAA0D0CADxxAE_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to OpaqueFunction.inAndOut(x : (A) -> B) -> (A) -> B
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction14OpaqueFunctionC18variantOptionalityq_xcq_xcSg1x_tFAA0D0CADxSgxAF_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to OpaqueFunction.variantOptionality(x : (A) -> B?) -> (A) -> B
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction14OpaqueFunctionCACyxq_Gycfc	// OpaqueFunction.init() -> OpaqueFunction<A, B>

// Optionality change of function.

// CHECK-NEXT:  #OpaqueFunction.variantOptionality!1: <U, V> (OpaqueFunction<U, V>) -> (((U) -> V)?) -> (U) -> V : _T027vtable_thunks_reabstraction14OpaqueFunctionC18variantOptionalityq_xcq_xcSg1x_tF	// OpaqueFunction.variantOptionality(x : (A) -> B?) -> (A) -> B

// CHECK-NEXT:   #OpaqueFunction.deinit!deallocator: _T027vtable_thunks_reabstraction14OpaqueFunctionCfD	// OpaqueFunction.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteFunction: Opaque<(S) -> S> {
  override func inAndOut(x: @escaping (S) -> S) -> (S) -> S { return x }
  override func variantOptionality(x: ((S) -> S)?) -> (S) -> S { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteFunction {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction16ConcreteFunctionC8inAndOutAA1SVAFcA2Fc1x_tFAA6OpaqueCADxxAG_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to ConcreteFunction.inAndOut(x : (S) -> S) -> (S) -> S
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction16ConcreteFunctionC18variantOptionalityAA1SVAFcA2FcSg1x_tFAA6OpaqueCADxSgxAH_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to ConcreteFunction.variantOptionality(x : (S) -> S?) -> (S) -> S
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction16ConcreteFunctionCACycfc	// ConcreteFunction.init() -> ConcreteFunction

// Optionality change of function.

// CHECK-NEXT:   #ConcreteFunction.variantOptionality!1: (ConcreteFunction) -> (((S) -> S)?) -> (S) -> S : _T027vtable_thunks_reabstraction16ConcreteFunctionC18variantOptionalityAA1SVAFcA2FcSg1x_tF	// ConcreteFunction.variantOptionality(x : (S) -> S?) -> (S) -> S

// CHECK-NEXT:   #ConcreteFunction.deinit!deallocator: _T027vtable_thunks_reabstraction16ConcreteFunctionCfD	// ConcreteFunction.__deallocating_deinit
// CHECK-NEXT: }

class OpaqueMetatype<U>: Opaque<U.Type> {
  override func inAndOut(x: U.Type) -> U.Type { return x }
  override func variantOptionality(x: U.Type?) -> U.Type { return x! }
}

// CHECK-LABEL: sil_vtable OpaqueMetatype {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction14OpaqueMetatypeC8inAndOutxmxm1x_tFAA0D0CADxxAE_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to OpaqueMetatype.inAndOut(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction14OpaqueMetatypeC18variantOptionalityxmxmSg1x_tFAA0D0CADxSgxAF_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to OpaqueMetatype.variantOptionality(x : A.Type?) -> A.Type
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction14OpaqueMetatypeCACyxGycfc	// OpaqueMetatype.init() -> OpaqueMetatype<A>

// Optionality change of metatype.

// CHECK-NEXT:   #OpaqueMetatype.variantOptionality!1: <U> (OpaqueMetatype<U>) -> (U.Type?) -> U.Type : _T027vtable_thunks_reabstraction14OpaqueMetatypeC18variantOptionalityxmxmSg1x_tF	// OpaqueMetatype.variantOptionality(x : A.Type?) -> A.Type

// CHECK-NEXT:   #OpaqueMetatype.deinit!deallocator: _T027vtable_thunks_reabstraction14OpaqueMetatypeCfD	// OpaqueMetatype.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteValueMetatype: Opaque<S.Type> {
  override func inAndOut(x: S.Type) -> S.Type { return x }
  override func variantOptionality(x: S.Type?) -> S.Type { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteValueMetatype {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction21ConcreteValueMetatypeC8inAndOutAA1SVmAFm1x_tFAA6OpaqueCADxxAG_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to ConcreteValueMetatype.inAndOut(x : S.Type) -> S.Type
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction21ConcreteValueMetatypeC18variantOptionalityAA1SVmAFmSg1x_tFAA6OpaqueCADxSgxAH_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to ConcreteValueMetatype.variantOptionality(x : S.Type?) -> S.Type
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction21ConcreteValueMetatypeCACycfc	// ConcreteValueMetatype.init() -> ConcreteValueMetatype

// Optionality change of metatype.

// CHECK-NEXT:   #ConcreteValueMetatype.variantOptionality!1: (ConcreteValueMetatype) -> (S.Type?) -> S.Type : _T027vtable_thunks_reabstraction21ConcreteValueMetatypeC18variantOptionalityAA1SVmAFmSg1x_tF	// ConcreteValueMetatype.variantOptionality(x : S.Type?) -> S.Type

// CHECK-NEXT:   #ConcreteValueMetatype.deinit!deallocator: _T027vtable_thunks_reabstraction21ConcreteValueMetatypeCfD	// ConcreteValueMetatype.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteClassMetatype: Opaque<C.Type> {
  override func inAndOut(x: C.Type) -> C.Type { return x }
  override func variantOptionality(x: C.Type?) -> C.Type { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteClassMetatype {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction21ConcreteClassMetatypeC8inAndOutAA1CCmAFm1x_tFAA6OpaqueCADxxAG_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to ConcreteClassMetatype.inAndOut(x : C.Type) -> C.Type
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : hidden _T027vtable_thunks_reabstraction21ConcreteClassMetatypeC18variantOptionalityAA1CCmAFmSg1x_tFAA6OpaqueCADxSgxAH_tFTV	// vtable thunk for Opaque.variantOptionality(x : A) -> A? dispatching to ConcreteClassMetatype.variantOptionality(x : C.Type?) -> C.Type
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction21ConcreteClassMetatypeCACycfc	// ConcreteClassMetatype.init() -> ConcreteClassMetatype

// Class metatypes are ABI compatible with optional class metatypes.

// CHECK-NEXT:   #ConcreteClassMetatype.deinit!deallocator: _T027vtable_thunks_reabstraction21ConcreteClassMetatypeCfD	// ConcreteClassMetatype.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteOptional: Opaque<S?> {
  override func inAndOut(x: S?) -> S? { return x }

  // FIXME: Should we allow this override in Sema?
  // override func variantOptionality(x: S??) -> S? { return x! }
}

// CHECK-LABEL: sil_vtable ConcreteOptional {
// CHECK-NEXT:   #Opaque.inAndOut!1: <T> (Opaque<T>) -> (T) -> T : hidden _T027vtable_thunks_reabstraction16ConcreteOptionalC8inAndOutAA1SVSgAG1x_tFAA6OpaqueCADxxAH_tFTV	// vtable thunk for Opaque.inAndOut(x : A) -> A dispatching to ConcreteOptional.inAndOut(x : S?) -> S?
// CHECK-NEXT:   #Opaque.inAndOutGeneric!1: <T><U> (Opaque<T>) -> (T, U) -> U : _T027vtable_thunks_reabstraction6OpaqueC15inAndOutGenericqd__x1x_qd__1ytlF	// Opaque.inAndOutGeneric<A> (x : A, y : A1) -> A1
// CHECK-NEXT:   #Opaque.inAndOutMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutMetatypesxmxm1x_tF	// Opaque.inAndOutMetatypes(x : A.Type) -> A.Type
// CHECK-NEXT:   #Opaque.inAndOutFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> (T) -> T : _T027vtable_thunks_reabstraction6OpaqueC17inAndOutFunctionsxxcxxc1x_tF	// Opaque.inAndOutFunctions(x : (A) -> A) -> (A) -> A
// CHECK-NEXT:   #Opaque.inAndOutTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T)) : _T027vtable_thunks_reabstraction6OpaqueC14inAndOutTuplesx_xm_xxcttx_xm_xxctt1x_tF	// Opaque.inAndOutTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))
// CHECK-NEXT:   #Opaque.variantOptionality!1: <T> (Opaque<T>) -> (T) -> T? : _T027vtable_thunks_reabstraction6OpaqueC18variantOptionalityxSgx1x_tF	// Opaque.variantOptionality(x : A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityMetatypes!1: <T> (Opaque<T>) -> (T.Type) -> T.Type? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityMetatypesxmSgxm1x_tF	// Opaque.variantOptionalityMetatypes(x : A.Type) -> A.Type?
// CHECK-NEXT:   #Opaque.variantOptionalityFunctions!1: <T> (Opaque<T>) -> (@escaping (T) -> T) -> ((T) -> T)? : _T027vtable_thunks_reabstraction6OpaqueC27variantOptionalityFunctionsxxcSgxxc1x_tF	// Opaque.variantOptionalityFunctions(x : (A) -> A) -> (A) -> A?
// CHECK-NEXT:   #Opaque.variantOptionalityTuples!1: <T> (Opaque<T>) -> ((T, (T.Type, (T) -> T))) -> (T, (T.Type, (T) -> T))? : _T027vtable_thunks_reabstraction6OpaqueC24variantOptionalityTuplesx_xm_xxcttSgx_xm_xxctt1x_tF	// Opaque.variantOptionalityTuples(x : (A, (A.Type, (A) -> A))) -> (A, (A.Type, (A) -> A))?
// CHECK-NEXT:   #Opaque.init!initializer.1: <T> (Opaque<T>.Type) -> () -> Opaque<T> : _T027vtable_thunks_reabstraction16ConcreteOptionalCACycfc	// ConcreteOptional.init() -> ConcreteOptional
// CHECK-NEXT:   #ConcreteOptional.deinit!deallocator: _T027vtable_thunks_reabstraction16ConcreteOptionalCfD	// ConcreteOptional.__deallocating_deinit
// CHECK-NEXT: }

// Make sure we remap the method's innermost generic parameters
// to the correct depth
class GenericBase<T> {
  func doStuff<U>(t: T, u: U) {}
  init<U>(t: T, u: U) {}
}

// CHECK-LABEL: sil_vtable GenericBase {
// CHECK-NEXT:   #GenericBase.doStuff!1: <T><U> (GenericBase<T>) -> (T, U) -> () : _T027vtable_thunks_reabstraction11GenericBaseC7doStuffyx1t_qd__1utlF	// GenericBase.doStuff<A> (t : A, u : A1) -> ()
// CHECK-NEXT:   #GenericBase.init!initializer.1: <T><U> (GenericBase<T>.Type) -> (T, U) -> GenericBase<T> : _T027vtable_thunks_reabstraction11GenericBaseCACyxGx1t_qd__1utclufc	// GenericBase.init<A> (t : A, u : A1) -> GenericBase<A>
// CHECK-NEXT:   #GenericBase.deinit!deallocator: _T027vtable_thunks_reabstraction11GenericBaseCfD	// GenericBase.__deallocating_deinit
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
// CHECK-NEXT:   #GenericBase.doStuff!1: <T><U> (GenericBase<T>) -> (T, U) -> () : hidden _T027vtable_thunks_reabstraction11ConcreteSubC7doStuffySi1t_x1utlFAA11GenericBaseCADyxAE_qd__AFtlFTV	// vtable thunk for GenericBase.doStuff<A> (t : A, u : A1) -> () dispatching to ConcreteSub.doStuff<A> (t : Int, u : A) -> ()
// CHECK-NEXT:   #GenericBase.init!initializer.1: <T><U> (GenericBase<T>.Type) -> (T, U) -> GenericBase<T> : hidden _T027vtable_thunks_reabstraction11ConcreteSubCACSi1t_x1utclufcAA11GenericBaseCAGyxGxAD_qd__AEtclufcTV	// vtable thunk for GenericBase.init<A> (t : A, u : A1) -> GenericBase<A> dispatching to ConcreteSub.init<A> (t : Int, u : A) -> ConcreteSub
// CHECK-NEXT:   #ConcreteSub.deinit!deallocator: _T027vtable_thunks_reabstraction11ConcreteSubCfD	// ConcreteSub.__deallocating_deinit
// CHECK-NEXT: }

class ConcreteBase {
  init<U>(t: Int, u: U) {}
  func doStuff<U>(t: Int, u: U) {}
}

// CHECK-LABEL: sil_vtable ConcreteBase {
// CHECK-NEXT:   #ConcreteBase.init!initializer.1: <U> (ConcreteBase.Type) -> (Int, U) -> ConcreteBase : _T027vtable_thunks_reabstraction12ConcreteBaseCACSi1t_x1utclufc	// ConcreteBase.init<A> (t : Int, u : A) -> ConcreteBase
// CHECK-NEXT:   #ConcreteBase.doStuff!1: <U> (ConcreteBase) -> (Int, U) -> () : _T027vtable_thunks_reabstraction12ConcreteBaseC7doStuffySi1t_x1utlF	// ConcreteBase.doStuff<A> (t : Int, u : A) -> ()
// CHECK-NEXT:   #ConcreteBase.deinit!deallocator: _T027vtable_thunks_reabstraction12ConcreteBaseCfD	// ConcreteBase.__deallocating_deinit
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
// CHECK-NEXT:   #ConcreteBase.init!initializer.1: <U> (ConcreteBase.Type) -> (Int, U) -> ConcreteBase : _T027vtable_thunks_reabstraction10GenericSubCACyxGSi1t_qd__1utclufc	// GenericSub.init<A> (t : Int, u : A1) -> GenericSub<A>
// CHECK-NEXT:   #ConcreteBase.doStuff!1: <U> (ConcreteBase) -> (Int, U) -> () : _T027vtable_thunks_reabstraction10GenericSubC7doStuffySi1t_qd__1utlF	// GenericSub.doStuff<A> (t : Int, u : A1) -> ()
// CHECK-NEXT:   #GenericSub.deinit!deallocator: _T027vtable_thunks_reabstraction10GenericSubCfD	// GenericSub.__deallocating_deinit
// CHECK-NEXT: }

// Issue with generic parameter index
class MoreGenericSub1<T, TT> : GenericBase<T> {
  override func doStuff<U>(t: T, u: U) {
    super.doStuff(t: t, u: u)
  }
}

// CHECK-LABEL: sil_vtable MoreGenericSub1 {
// CHECK-NEXT:   #GenericBase.doStuff!1: <T><U> (GenericBase<T>) -> (T, U) -> () : _T027vtable_thunks_reabstraction15MoreGenericSub1C7doStuffyx1t_qd__1utlF	// MoreGenericSub1.doStuff<A> (t : A, u : A1) -> ()
// CHECK-NEXT:   #GenericBase.init!initializer.1: <T><U> (GenericBase<T>.Type) -> (T, U) -> GenericBase<T> : _T027vtable_thunks_reabstraction11GenericBaseCACyxGx1t_qd__1utclufc	// GenericBase.init<A> (t : A, u : A1) -> GenericBase<A>
// CHECK-NEXT:   #MoreGenericSub1.deinit!deallocator: _T027vtable_thunks_reabstraction15MoreGenericSub1CfD	// MoreGenericSub1.__deallocating_deinit
// CHECK-NEXT: }

class MoreGenericSub2<TT, T> : GenericBase<T> {
  override func doStuff<U>(t: T, u: U) {
    super.doStuff(t: t, u: u)
  }
}

// CHECK-LABEL: sil_vtable MoreGenericSub2 {
// CHECK-NEXT:   #GenericBase.doStuff!1: <T><U> (GenericBase<T>) -> (T, U) -> () : _T027vtable_thunks_reabstraction15MoreGenericSub2C7doStuffyq_1t_qd__1utlF	// MoreGenericSub2.doStuff<A> (t : B, u : A1) -> ()
// CHECK-NEXT:   #GenericBase.init!initializer.1: <T><U> (GenericBase<T>.Type) -> (T, U) -> GenericBase<T> : _T027vtable_thunks_reabstraction11GenericBaseCACyxGx1t_qd__1utclufc	// GenericBase.init<A> (t : A, u : A1) -> GenericBase<A>
// CHECK-NEXT:   #MoreGenericSub2.deinit!deallocator: _T027vtable_thunks_reabstraction15MoreGenericSub2CfD	// MoreGenericSub2.__deallocating_deinit
// CHECK-NEXT: }
