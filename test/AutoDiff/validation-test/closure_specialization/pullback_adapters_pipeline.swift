// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name src %s -o - | %FileCheck %s

import _Differentiation

// CHECK-LABEL:      {{^}}enum _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0_spec_bb1_1_a56f15d406842689d181fc01e760de89 {
// CHECK-NEXT:         case bb2((predecessor: _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb2__Pred__src_0_wrt_0))
// CHECK:              case bb1((predecessor: _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb1__Pred__src_0_wrt_0, (_: @callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>)))
// CHECK-LABEL:      {{^}}}

// MARK: - Function conversion around direct and adapted pullbacks

// SILCombine can fold the closure application once ADCS moves it into the pullback.

@differentiable(reverse, wrt: x)
@inline(never)
public func tagged(_ x: Float, _ label: String) -> Float { x * x }

// Keep debug uses of both the @Sendable closure and its converted value.
@derivative(of: tagged, wrt: x)
@inline(__always)
public func vjpTagged(_ x: Float, _ label: String)
  -> (value: Float, pullback: (Float) -> Float) {
  let callback: @Sendable (Float) -> Float = { seed in
    withExtendedLifetime(label) { 2 * x * seed }
  }
  let converted: (Float) -> Float = callback
  return (x * x, converted)
}

// CHECK-LABEL:      {{^}}// reverse-mode derivative of branchConverted(_:_:_:)
// CHECK-NOT:          = partial_apply
// CHECK-NOT:          = convert_function
// CHECK:              // function_ref specialized pullback of branchConverted(_:_:_:)
// CHECK-NEXT:         %[[#FN:]] = function_ref @$s3src15branchConvertedyS2f_SSSbtFTJpSUUpSr00180_43$s3src15branchConvertedyS2f_SSSbtFTJrSUUpSr$s3src051_AD__$s3src15branchConvertedyS2f_SSSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_7290bce8659ab024d29afd5d6068f5ebOD_AGCDHeryfDeTf1nb_n :
// CHECK-NEXT:         %[[#PB:]] = partial_apply {{.*}} %[[#FN]]
// CHECK-NEXT:         %[[#RESULT:]] = tuple (%[[#]], %[[#PB]])
// CHECK-NEXT:         return %[[#RESULT]]
// CHECK:            } // end sil function
// CHECK-LABEL:      {{^}}// specialized pullback of branchConverted(_:_:_:)
// CHECK-NOT:          = partial_apply
// CHECK-NOT:          apply {{.*}} : $@callee_guaranteed
// CHECK:            } // end sil function

@differentiable(reverse, wrt: x)
public func branchConverted(_ x: Float, _ label: String, _ flag: Bool) -> Float {
  if flag { return tagged(x, label) }
  return x + x
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func taggedPair(_ x: Float, _ y: Float, _ label: String) -> Float {
  x * x + y * y
}

@derivative(of: taggedPair, wrt: (x, y))
@inline(__always)
public func vjpTaggedPair(_ x: Float, _ y: Float, _ label: String)
  -> (value: Float, pullback: (Float) -> (Float, Float)) {
  // Here the debug use belongs to the converted callback.
  let callback: (Float) -> (Float, Float) = { @Sendable seed in
    withExtendedLifetime(label) { (2 * x * seed, 2 * y * seed) }
  }
  return (x * x + y * y, callback)
}

// CHECK-LABEL:      {{^}}// reverse-mode derivative of branchSubset(_:_:_:_:)
// CHECK-NOT:          = partial_apply
// CHECK-NOT:          = convert_function
// CHECK:              // function_ref specialized pullback of branchSubset(_:_:_:_:)
// CHECK-NEXT:         %[[#FN:]] = function_ref @$s3src12branchSubsetyS2f_SfSSSbtFTJpSUUUpSr00179_43$s3src12branchSubsetyS2f_SfSSSbtFTJrSUUUpSr$s3src050_AD__$s3src12branchSubsetyS2f_SfSSSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_aa09c675f24dbd14b20c809278e998b5OD_EDHIGeoxfCeTf1nb_n :
// CHECK-NEXT:         %[[#PB:]] = partial_apply {{.*}} %[[#FN]]
// CHECK-NEXT:         %[[#RESULT:]] = tuple (%[[#]], %[[#PB]])
// CHECK-NEXT:         return %[[#RESULT]]
// CHECK:            } // end sil function
// CHECK-LABEL:      {{^}}// specialized pullback of branchSubset(_:_:_:_:)
// CHECK-NOT:          = partial_apply
// CHECK-NOT:          apply {{.*}} : $@callee_guaranteed
// CHECK:            } // end sil function

@differentiable(reverse, wrt: x)
public func branchSubset(_ x: Float, _ y: Float, _ label: String, _ flag: Bool) -> Float {
  if flag { return taggedPair(x, y, label) }
  return x + x
}

@differentiable(reverse, wrt: x)
@inline(never)
public func genericTagged<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String
) -> T where T.TangentVector == T {
  x + x
}

@derivative(of: genericTagged, wrt: x)
@inline(__always)
public func vjpGenericTagged<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String
) -> (value: T, pullback: (T) -> T) where T.TangentVector == T {
  (x + x, { @Sendable seed in
    withExtendedLifetime(label) { seed + seed }
  })
}

@differentiable(reverse, wrt: x)
public func branchGeneric<T: BinaryFloatingPoint & Differentiable & Sendable>(
  _ x: T, _ label: String, _ flag: Bool
) -> T where T.TangentVector == T {
  if flag { return genericTagged(x, label) }
  return x + x
}

// MARK: - Generic and thin subset adapters

// CHECK-LABEL:      {{^}}// reverse-mode derivative of genericSubset<A>
// CHECK-NOT:        } // end sil function
// CHECK:              // function_ref specialized pullback of genericSubset<A>
// CHECK-NEXT:         %[[#PB:]] = function_ref @$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlFAcDRzAGRszlTJpSUUpSr00332_112$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlFAcDRzAGRszlTJrSUUpSr$s3src0109_AD__$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlF_bb3__Pred__a12_0_wrt_0_16_ghijk6AaBPQzN50_spec_bb2_1_bb1_1_0fefac2a6e4a1e89acefe4ad00dba713OyxGD_snAEFloHjfjaTf1nb_n :
// CHECK-NEXT:         %[[#PULLBACK:]] = partial_apply {{.*}} %[[#PB]]{{[<(]}}
// CHECK-NEXT:         %[[#RESULT:]] = tuple (%[[#]], %[[#PULLBACK]])
// CHECK-NOT:        } // end sil function
// CHECK:              return %[[#RESULT]]
// CHECK-NEXT:       } // end sil function

// CHECK-LABEL:      {{^}}// specialized pullback of genericSubset<A>
// CHECK-NOT:          = partial_apply
// CHECK-NOT:          apply {{.*}} : $@callee_guaranteed
// CHECK:            } // end sil function

// Fixing the generic seed to Float requires reabstraction. Differentiating
// only x adds an outer subset thunk.

@inline(__always)
func makePullback<Seed: BinaryFloatingPoint, T: AdditiveArithmetic>(
  _: T.Type, _: Seed.Type
) -> (Seed) -> (Float, T) {
  { seed in (2 * Float(seed), T.zero) }
}

@differentiable(reverse, wrt: (x, y))
@inline(never)
public func mixed<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T) -> Float
where T.TangentVector == T { 2 * x }

@derivative(of: mixed, wrt: (x, y))
@inline(__always)
public func vjpMixed<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T)
  -> (value: Float, pullback: (Float) -> (Float, T)) where T.TangentVector == T {
  (2 * x, makePullback(T.self, Float.self))
}

@differentiable(reverse, wrt: x)
public func genericSubset<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T, _ flag: Bool) -> Float
where T.TangentVector == T {
  if flag { return mixed(x, y) }
  return x * x
}

// Differentiating only x adds a subset thunk around a captureless pullback.
@differentiable(reverse, wrt: x)
public func thinSubset(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  if flag { return x + y }
  return x * x
}

// MARK: - Adapting an indirect Never error result

// Instantiating the error type with Never leaves an indirect error result.
// A nonthrowing reabstraction thunk adapts the root to the pullback type.
@inline(__always)
public func genericPullback<E: Error>(_ x: Float, _: E.Type)
  -> (Float) throws(E) -> Float {
  { (seed: Float) throws(E) -> Float in 2 * x * seed }
}

@differentiable(reverse)
public func genericNeverSquare(_ x: Float) -> Float { x * x }

@derivative(of: genericNeverSquare)
@inline(__always)
public func vjpGenericNeverSquare(_ x: Float)
  -> (value: Float, pullback: (Float) -> Float) {
  (x * x, genericPullback(x, Never.self))
}

@differentiable(reverse, wrt: x)
public func branchGenericNever(_ x: Float, _ flag: Bool) -> Float {
  if flag { return genericNeverSquare(x) }
  return x + x
}

// MARK: - A runtime wrapper around a known pullback

// CHECK-LABEL:      // reverse-mode derivative of branchDynamic(_:_:_:_:)
// CHECK:              // function_ref closure #1 in vjpRuntimeWrapped(_:_:_:)
// CHECK-NEXT:         %[[#INNER_FN:]] = function_ref @$s3src17vjpRuntimeWrappedySf5value_S2fc8pullbacktSf_SSS2f_S2fXEtctFS2fcfU_ :
// CHECK-NEXT:         %[[#INNER:]] = partial_apply [callee_guaranteed] %[[#INNER_FN]]
// CHECK-NOT:          = partial_apply
// CHECK:              %[[#CAPTURES:]] = tuple (%[[#]], %[[#INNER]])
// CHECK-NEXT:         %[[#PAYLOAD:]] = tuple {{.*}} (%[[#]], %[[#CAPTURES]])
// CHECK-NEXT:         enum $_AD__$s3src13branchDynamicyS2f_SSSbS2f_S2fXEtctF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_ca399df85864bf0fc710b97ef37bb11f, #_AD__$s3src13branchDynamicyS2f_SSSbS2f_S2fXEtctF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_ca399df85864bf0fc710b97ef37bb11f.bb1!enumelt, %[[#PAYLOAD]]
// CHECK-NOT:          = partial_apply
// CHECK:              // function_ref specialized pullback of branchDynamic(_:_:_:_:)
// CHECK-NEXT:         %[[#OUTER_FN:]] = function_ref @$s3src13branchDynamicyS2f_SSSbS2f_S2fXEtctFTJpSUUUpSr00198_53$s3src13branchDynamicyS2f_SSSbS2f_S2fXEtctFTJrSUUUpSr$s3src035_AD__$s3src13branchDynamicyS2f_SSSbe1_e18XEtctF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_ca399df85864bf0fc710b97ef37bb11fOD_yoEEFfbggmfTf1nb_n :
// CHECK-NEXT:         %[[#OUTER:]] = partial_apply [callee_guaranteed] %[[#OUTER_FN]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3src13branchDynamicyS2f_SSSbS2f_S2fXEtctF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_ca399df85864bf0fc710b97ef37bb11f) -> Float
// CHECK-NEXT:         %[[#RESULT:]] = tuple (%[[#]], %[[#OUTER]])
// CHECK-NEXT:         return %[[#RESULT]]
// CHECK-NEXT:       } // end sil function
// CHECK-LABEL:      // specialized pullback of branchDynamic(_:_:_:_:)
// CHECK-NOT:          = partial_apply
// CHECK-NOT:          apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK:              switch_enum %[[#]], case #_AD__$s3src13branchDynamicyS2f_SSSbS2f_S2fXEtctF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_ca399df85864bf0fc710b97ef37bb11f.bb2!enumelt: bb[[#]], case #_AD__$s3src13branchDynamicyS2f_SSSbS2f_S2fXEtctF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_ca399df85864bf0fc710b97ef37bb11f.bb1!enumelt: bb[[#WRAPPED_CASE:]]
// CHECK-NOT:          = partial_apply
// CHECK-NOT:          apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK:            bb[[#WRAPPED_CASE]](%[[#WRAPPED_PAYLOAD:]] :
// CHECK-NEXT:         %[[#STORED:]] = tuple_extract %[[#WRAPPED_PAYLOAD]], 1
// CHECK-NEXT:         %[[#WRAPPER:]] = tuple_extract %[[#STORED]], 0
// CHECK-NEXT:         %[[#SAVED_INNER:]] = tuple_extract %[[#STORED]], 1
// CHECK:              %[[#NOESCAPE:]] = convert_escape_to_noescape %[[#SAVED_INNER]]
// CHECK-NEXT:         apply %[[#WRAPPER]](%[[#]], %[[#NOESCAPE]])
// CHECK-NOT:          = partial_apply
// CHECK-NOT:          apply {{.*}} : $@callee_guaranteed (Float) -> Float
// CHECK:            } // end sil function

public typealias Pullback = (Float) -> Float
public typealias Wrapper = (Float, Pullback) -> Float

@differentiable(reverse, wrt: x)
@inline(never)
public func runtimeWrapped(_ x: Float, _ label: String, _ wrapper: @escaping Wrapper) -> Float { 2*x }

@derivative(of: runtimeWrapped, wrt: x)
@inline(__always)
public func vjpRuntimeWrapped(_ x: Float, _ label: String, _ wrapper: @escaping Wrapper)
  -> (value: Float, pullback: Pullback) {
  let pullback: Pullback = { seed in withExtendedLifetime(label) { 2*seed } }
  return (2*x, { seed in wrapper(seed, pullback) })
}

@differentiable(reverse, wrt: x)
public func branchDynamic(_ x: Float, _ label: String, _ flag: Bool, _ wrapper: @escaping Wrapper) -> Float {
  if flag { return runtimeWrapped(x, label, wrapper) }
  return x*x
}

// MARK: - A generic ABI thunk with a dynamically captured pullback

// CHECK-LABEL:      {{^}}// reverse-mode derivative of lookup(_:_:)
// CHECK:              // function_ref specialized pullback of lookup(_:_:)
// CHECK-NEXT:         %[[#PB:]] = function_ref @$s3src6lookupySdSgSDySSSdG_SStFTJpSUpSr00167_39$s3src6lookupySdSgSDySSSdG_SStFTJrSUpSr$s3src048_AD__$s3src6lookupySdSgSDySSSdG_SStF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_a56f15d406842689d181fc01e760de89OD_iEGEBenkfqeTf1nb_n : $@convention(thin) (Optional<Double>.TangentVector, @owned _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0_spec_bb1_1_a56f15d406842689d181fc01e760de89) -> @owned Dictionary<String, Double>
// CHECK-NEXT:         %[[#]] = partial_apply [callee_guaranteed] %[[#PB]]
// CHECK:            } // end sil function
// CHECK-LABEL:      {{^}}// specialized pullback of lookup(_:_:)
// CHECK:            sil private @$s3src6lookupySdSgSDySSSdG_SStFTJpSUpSr00167_39$s3src6lookupySdSgSDySSSdG_SStFTJrSUpSr$s3src048_AD__$s3src6lookupySdSgSDySSSdG_SStF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_a56f15d406842689d181fc01e760de89OD_iEGEBenkfqeTf1nb_n :
// CHECK:            bb[[#]](%[[#PAYLOAD:]] : $(predecessor: _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb1__Pred__src_0_wrt_0, (_: @callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>))):
// CHECK:              %[[#CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK:              %[[#CALLBACK:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK:              apply %[[#CALLBACK]]({{.*}}) : $@callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>
// CHECK:            } // end sil function

extension Dictionary: Differentiable where Value: Differentiable {
  public typealias TangentVector = [Key: Value.TangentVector]
  public mutating func move(by direction: TangentVector) {
    for (key, component) in direction { self[key]!.move(by: component) }
  }
}

extension Dictionary: AdditiveArithmetic where Value: AdditiveArithmetic {
  public static var zero: Self { [:] }
  public static func + (_ lhs: Self, _ rhs: Self) -> Self {
    lhs.merging(rhs, uniquingKeysWith: +)
  }
  public static func - (_ lhs: Self, _ rhs: Self) -> Self {
    lhs.merging(rhs.mapValues { .zero - $0 }, uniquingKeysWith: +)
  }
}

extension Dictionary where Value: Differentiable {
  @usableFromInline
  @derivative(of: subscript(_:))
  func vjpSubscriptGet(key: Key)
    -> (value: Value?, pullback: (Optional<Value>.TangentVector) -> TangentVector) {
    (self[key], { tangent in
      if let value = tangent.value { return [key: value] }
      return .zero
    })
  }
}

@inline(never)
private func hasEntries(_ values: [String: Double]) -> Bool {
  !values.isEmpty
}

@inline(never)
private func hasKey(_ values: [String: Double], _ key: String) -> Bool {
  values.keys.contains(key)
}

// Inlining merges the two false paths, which forward an empty tape payload.
@inline(__always)
private func containsKey(_ values: [String: Double], _ key: String) -> Bool {
  guard hasEntries(values) else { return false }
  return hasKey(values, key)
}

// ADCS must keep the callback captured by the ABI thunk from generic specialization.
@differentiable(reverse)
public func lookup(_ values: [String: Double], _ key: String) -> Double? {
  if containsKey(values, key) { return values[key] }
  return nil
}

@differentiable(reverse)
public func lookupFirst(_ values: [String: Double]) -> Double {
  lookup(values, "first")!
}
