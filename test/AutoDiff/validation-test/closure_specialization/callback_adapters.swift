// REQUIRES: executable_test
// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-O)
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil

import _Differentiation
import StdlibUnittest

var tests = TestSuite("CallbackAdapters")

// MARK: - Dictionary reabstraction

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

// Generic specialization adds an ABI thunk that captures the dictionary pullback.
// The direct membership test has one false predecessor.
// CHECK-LABEL:     {{^}}enum _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0_spec_bb1_1_9ad8e9df5abb015c018af33e813594f5 {
// CHECK-NEXT:        case bb2((predecessor: _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb2__Pred__src_0_wrt_0))
// CHECK:             case bb1((predecessor: _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb1__Pred__src_0_wrt_0, (_: @callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>)))
// CHECK-LABEL:     {{^}}}
// CHECK-LABEL:     {{^}}// reverse-mode derivative of lookup(_:_:)
// CHECK:             // function_ref specialized pullback of lookup(_:_:)
// CHECK-NEXT:        %[[#PB:]] = function_ref @$s3src6lookupySdSgSDySSSdG_SStFTJpSUpSr00167_39$s3src6lookupySdSgSDySSSdG_SStFTJrSUpSr$s3src048_AD__$s3src6lookupySdSgSDySSSdG_SStF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_9ad8e9df5abb015c018af33e813594f5OD_iEGEBenkfqeTf1nb_n : $@convention(thin) (Optional<Double>.TangentVector, @owned _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb3__Pred__src_0_wrt_0_spec_bb1_1_9ad8e9df5abb015c018af33e813594f5) -> @owned Dictionary<String, Double>
// CHECK-NEXT:        %[[#]] = partial_apply [callee_guaranteed] %[[#PB]]
// CHECK:           } // end sil function
// CHECK-LABEL:     {{^}}// specialized pullback of lookup(_:_:)
// CHECK:           sil private @$s3src6lookupySdSgSDySSSdG_SStFTJpSUpSr00167_39$s3src6lookupySdSgSDySSSdG_SStFTJrSUpSr$s3src048_AD__$s3src6lookupySdSgSDySSSdG_SStF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_9ad8e9df5abb015c018af33e813594f5OD_iEGEBenkfqeTf1nb_n
// CHECK:           bb[[#]](%[[#PAYLOAD:]] : $(predecessor: _AD__$s3src6lookupySdSgSDySSSdG_SStF_bb1__Pred__src_0_wrt_0, (_: @callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>))):
// CHECK:             %[[#CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK:             %[[#CALLBACK:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK:             apply %[[#CALLBACK]]({{.*}}) : $@callee_guaranteed (@in_guaranteed Optional<Double>.TangentVector) -> @owned Dictionary<String, Double>
// CHECK:           } // end sil function
@differentiable(reverse)
public func lookup(_ values: [String: Double], _ key: String) -> Double? {
  if values.keys.contains(key) { return values[key] }
  return nil
}

@differentiable(reverse)
public func lookupFirst(_ values: [String: Double]) -> Double {
  lookup(values, "first")!
}

tests.test("DictionaryReabstraction") {
  let result = valueWithPullback(at: ["first": 3.0], of: lookupFirst)
  precondition(result.value == 3)
  precondition(result.pullback(2) == ["first": 2])
  precondition(result.pullback(5) == ["first": 5])
}

// MARK: - Converted pullback callbacks

@differentiable(reverse, wrt: x)
@inline(never)
public func tagged(_ x: Float, _ label: String) -> Float { x * x }

// Naming both values gives the original and converted closures separate debug uses.
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

// CHECK-LABEL:     {{^}}// reverse-mode derivative of branchConverted(_:_:_:)
// CHECK:             %[[#DIRECT_PAYLOAD:]] = tuple $(predecessor: _AD__$s3src15branchConvertedyS2f_SSSbtF_bb1__Pred__src_0_wrt_0, (String, Float))
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src15branchConvertedyS2f_SSSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_503afea1521418755fed5d3b56dd5425, #_AD__$s3src15branchConvertedyS2f_SSSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_503afea1521418755fed5d3b56dd5425.bb1!enumelt, %[[#DIRECT_PAYLOAD]]
// CHECK:             // function_ref specialized pullback of branchConverted(_:_:_:)
// CHECK-NEXT:        %[[#DIRECT_PB:]] = function_ref @$s3src15branchConvertedyS2f_SSSbtFTJpSUUpSr00180_43$s3src15branchConvertedyS2f_SSSbtFTJrSUUpSr$s3src051_AD__$s3src15branchConvertedyS2f_SSSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_503afea1521418755fed5d3b56dd5425OD_AGCDHeryfDeTf1nb_n
// CHECK-NEXT:        %[[#]] = partial_apply [callee_guaranteed] %[[#DIRECT_PB]](%[[#]])
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of branchConverted(_:_:_:)
// CHECK:           sil private @$s3src15branchConvertedyS2f_SSSbtFTJpSUUpSr00180_43$s3src15branchConvertedyS2f_SSSbtFTJrSUUpSr$s3src051_AD__$s3src15branchConvertedyS2f_SSSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_503afea1521418755fed5d3b56dd5425OD_AGCDHeryfDeTf1nb_n
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

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
  // Name only the converted closure to give it the debug use.
  let callback: (Float) -> (Float, Float) = { @Sendable seed in
    withExtendedLifetime(label) { (2 * x * seed, 2 * y * seed) }
  }
  return (x * x + y * y, callback)
}

// The subset-parameters thunk captures the converted callback.
// CHECK-LABEL:     {{^}}// reverse-mode derivative of branchSubset(_:_:_:_:)
// CHECK:             %[[#SUBSET_PAYLOAD:]] = tuple $(predecessor: _AD__$s3src12branchSubsetyS2f_SfSSSbtF_bb1__Pred__src_0_wrt_0, (String, Float, Float))
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src12branchSubsetyS2f_SfSSSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_060a0d4f38eb9e309ea9e1b72862bc2f, #_AD__$s3src12branchSubsetyS2f_SfSSSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_060a0d4f38eb9e309ea9e1b72862bc2f.bb1!enumelt, %[[#SUBSET_PAYLOAD]]
// CHECK:             // function_ref specialized pullback of branchSubset(_:_:_:_:)
// CHECK-NEXT:        %[[#SUBSET_PB:]] = function_ref @$s3src12branchSubsetyS2f_SfSSSbtFTJpSUUUpSr00179_43$s3src12branchSubsetyS2f_SfSSSbtFTJrSUUUpSr$s3src050_AD__$s3src12branchSubsetyS2f_SfSSSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_060a0d4f38eb9e309ea9e1b72862bc2fOD_EDHIGeoxfCeTf1nb_n
// CHECK-NEXT:        %[[#]] = partial_apply [callee_guaranteed] %[[#SUBSET_PB]](%[[#]])
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of branchSubset(_:_:_:_:)
// CHECK:           sil private @$s3src12branchSubsetyS2f_SfSSSbtFTJpSUUUpSr00179_43$s3src12branchSubsetyS2f_SfSSSbtFTJrSUUUpSr$s3src050_AD__$s3src12branchSubsetyS2f_SfSSSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_060a0d4f38eb9e309ea9e1b72862bc2fOD_EDHIGeoxfCeTf1nb_n
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func branchSubset(_ x: Float, _ y: Float, _ label: String, _ flag: Bool) -> Float {
  if flag { return taggedPair(x, y, label) }
  return x + x
}

// An address-only callback also needs a generic reabstraction thunk.
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

// CHECK-LABEL:     {{^}}// reverse-mode derivative of branchGeneric<A>(_:_:_:)
// CHECK:             %[[#GEN_PAYLOAD:]] = tuple $(predecessor: _AD__$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlF_bb1__Pred__src_0_wrt_0_SBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAbCPQzRszl<τ_0_0>, (_: String))
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlF_bb3__Pred__src_0_wrt_0_SBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAbCPQzRszl_spec_bb2_1_bb1_1_5008d02e90884d16240fc672753dd6ab<τ_0_0>, #_AD__$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlF_bb3__Pred__src_0_wrt_0_SBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAbCPQzRszl_spec_bb2_1_bb1_1_5008d02e90884d16240fc672753dd6ab.bb1!enumelt, %[[#GEN_PAYLOAD]]
// CHECK:             // function_ref specialized pullback of branchGeneric<A>(_:_:_:)
// CHECK-NEXT:        %[[#GEN_PB:]] = function_ref @$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlFSBRzsACRzAdERzAHRszlTJpSUUpSr00374_137$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlFSBRzsACRzAdERzAHRszlTJrSUUpSr$s3src0125_AD__$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlF_bb3__Pred__a9_0_wrt_0_fgh1_ijklm6AbCPQzP50_spec_bb2_1_bb1_1_5008d02e90884d16240fc672753dd6abOyxGD_zDDDEnJilmkaTf1nnb_n
// CHECK-NEXT:        %[[#]] = partial_apply [callee_guaranteed] %[[#GEN_PB]]<τ_0_0>(%[[#]])
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of branchGeneric<A>(_:_:_:)
// CHECK:           sil private @$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlFSBRzsACRzAdERzAHRszlTJpSUUpSr00374_137$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlFSBRzsACRzAdERzAHRszlTJrSUUpSr$s3src0125_AD__$s3src13branchGenericyxx_SSSbtSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAdEPQzRszlF_bb3__Pred__a9_0_wrt_0_fgh1_ijklm6AbCPQzP50_spec_bb2_1_bb1_1_5008d02e90884d16240fc672753dd6abOyxGD_zDDDEnJilmkaTf1nnb_n
// CHECK:             %[[#CAPTURES:]] = tuple_extract %[[#]], 1
// CHECK:             %[[#LABEL:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK:             // function_ref closure #1 in vjpGenericTagged<A>(_:_:)
// CHECK-NEXT:        %[[#ROOT:]] = function_ref @$s3src16vjpGenericTaggedyx5value_xxc8pullbacktx_SStSBRzs8SendableRz16_Differentiation14DifferentiableRz13TangentVectorAfGPQzRszlFxxYbcfU_
// CHECK-NEXT:        %[[#CALLBACK:]] = partial_apply [callee_guaranteed] %[[#ROOT]]<τ_0_0>(%[[#LABEL]]) : $@convention(thin) @Sendable
// CHECK-NEXT:        %[[#CONVERTED:]] = convert_function %[[#CALLBACK]] to $@callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK-NEXT:        %[[#]] = apply %[[#CONVERTED]]({{.*}}) : $@callee_guaranteed (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:           } // end sil function

tests.test("ConvertedCallbacks") {
  let label = String(repeating: "a captured label ", count: 5)
  for flag in [false, true] {
    for x: Float in [-3, 0, 3] {
      let expectedDerivative: Float = flag ? 2 * x : 2
      let direct = valueWithPullback(at: x, of: { branchConverted($0, label, flag) })
      precondition(direct.value == (flag ? x * x : 2 * x))
      precondition(direct.pullback(1) == expectedDerivative)
      precondition(direct.pullback(3) == 3 * expectedDerivative)

      let subset = valueWithPullback(at: x, of: { branchSubset($0, 4, label, flag) })
      precondition(subset.value == (flag ? x * x + 16 : 2 * x))
      precondition(subset.pullback(1) == expectedDerivative)
      precondition(subset.pullback(3) == 3 * expectedDerivative)

      let genericFloat = valueWithPullback(at: x, of: { branchGeneric($0, label, flag) })
      precondition(genericFloat.value == 2 * x)
      precondition(genericFloat.pullback(1) == 2)
      precondition(genericFloat.pullback(3) == 6)

      let genericDouble = valueWithPullback(at: Double(x), of: { branchGeneric($0, label, flag) })
      precondition(genericDouble.value == 2 * Double(x))
      precondition(genericDouble.pullback(1) == 2)
      precondition(genericDouble.pullback(3) == 6)
    }
  }
}

// Fixing Seed to Float requires reabstraction. Differentiating only x adds a
// subset-parameters thunk.

// MARK: - Generic and thin subset callbacks

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

// CHECK-LABEL:     {{^}}// reverse-mode derivative of genericSubset<A>(_:_:_:)
// CHECK:             %[[#GEN_PAYLOAD:]] = tuple $(predecessor: _AD__$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlF_bb1__Pred__src_0_wrt_0_16_Differentiation14DifferentiableRz13TangentVectorAaBPQzRszl<τ_0_0>, ())
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlF_bb3__Pred__src_0_wrt_0_16_Differentiation14DifferentiableRz13TangentVectorAaBPQzRszl_spec_bb2_1_bb1_1_20a8edfd21f259fceb54bd092a61d3bc<τ_0_0>, #_AD__$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlF_bb3__Pred__src_0_wrt_0_16_Differentiation14DifferentiableRz13TangentVectorAaBPQzRszl_spec_bb2_1_bb1_1_20a8edfd21f259fceb54bd092a61d3bc.bb1!enumelt, %[[#GEN_PAYLOAD]]
// CHECK:             // function_ref specialized pullback of genericSubset<A>(_:_:_:)
// CHECK-NEXT:        %[[#GEN_PB:]] = function_ref @$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlFAcDRzAGRszlTJpSUUpSr00332_112$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlFAcDRzAGRszlTJrSUUpSr$s3src0109_AD__$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlF_bb3__Pred__a12_0_wrt_0_16_ghijk6AaBPQzN50_spec_bb2_1_bb1_1_20a8edfd21f259fceb54bd092a61d3bcOyxGD_snAEFloHjfjaTf1nb_n
// CHECK-NEXT:        %[[#]] = partial_apply [callee_guaranteed] %[[#GEN_PB]]<τ_0_0>(%[[#]])
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of genericSubset<A>(_:_:_:)
// CHECK:           sil private @$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlFAcDRzAGRszlTJpSUUpSr00332_112$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlFAcDRzAGRszlTJrSUUpSr$s3src0109_AD__$s3src13genericSubsetyS2f_xSbt16_Differentiation14DifferentiableRz13TangentVectorAcDPQzRszlF_bb3__Pred__a12_0_wrt_0_16_ghijk6AaBPQzN50_spec_bb2_1_bb1_1_20a8edfd21f259fceb54bd092a61d3bcOyxGD_snAEFloHjfjaTf1nb_n
// CHECK-NOT:         partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func genericSubset<T: Differentiable & AdditiveArithmetic>(_ x: Float, _ y: T, _ flag: Bool) -> Float
where T.TangentVector == T {
  if flag { return mixed(x, y) }
  return x * x
}

// Differentiating only x wraps addition's captureless pullback in a subset thunk.
@differentiable(reverse, wrt: x)
public func thinSubset(_ x: Float, _ y: Float, _ flag: Bool) -> Float {
  if flag { return x + y }
  return x * x
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of thinSubset(_:_:_:)
// CHECK:             %[[#THIN_PAYLOAD:]] = tuple $(predecessor: _AD__$s3src10thinSubsetyS2f_SfSbtF_bb1__Pred__src_0_wrt_0, ())
// CHECK-NEXT:        %[[#]] = enum $_AD__$s3src10thinSubsetyS2f_SfSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_430715bf138657888fc8e305c54983f2, #_AD__$s3src10thinSubsetyS2f_SfSbtF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_430715bf138657888fc8e305c54983f2.bb1!enumelt, %[[#THIN_PAYLOAD]]
// CHECK:             // function_ref specialized pullback of thinSubset(_:_:_:)
// CHECK-NEXT:        %[[#THIN_PB:]] = function_ref @$s3src10thinSubsetyS2f_SfSbtFTJpSUUpSr00170_38$s3src10thinSubsetyS2f_SfSbtFTJrSUUpSr$s3src046_AD__$s3src10thinSubsetyS2f_SfSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_430715bf138657888fc8e305c54983f2OD_FdCICewnfteTf1nb_n
// CHECK-NEXT:        %[[#]] = partial_apply [callee_guaranteed] %[[#THIN_PB]](%[[#]])
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of thinSubset(_:_:_:)
// CHECK:           sil private @$s3src10thinSubsetyS2f_SfSbtFTJpSUUpSr00170_38$s3src10thinSubsetyS2f_SfSbtFTJrSUUpSr$s3src046_AD__$s3src10thinSubsetyS2f_SfSbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_430715bf138657888fc8e305c54983f2OD_FdCICewnfteTf1nb_n
// CHECK-NOT:         partial_apply
// CHECK-NOT:         apply {{.*}} : $@callee_guaranteed
// CHECK:           } // end sil function

tests.test("SubsetPullback") {
  for flag in [false, true] {
    let floating = valueWithPullback(at: Float(3), of: {
      genericSubset($0, Float(4), flag)
    })
    expectEqual(flag ? 6 : 9, floating.value)
    expectEqual(flag ? 2 : 6, floating.pullback(1))
    expectEqual(flag ? 10 : 30, floating.pullback(5))

    let double = valueWithPullback(at: Float(3), of: {
      genericSubset($0, Double(4), flag)
    })
    expectEqual(flag ? 6 : 9, double.value)
    expectEqual(flag ? 2 : 6, double.pullback(1))
    expectEqual(flag ? 4 : 12, double.pullback(2))

    let thin = valueWithPullback(at: Float(3), of: { thinSubset($0, 4, flag) })
    expectEqual(flag ? 7 : 9, thin.value)
    expectEqual(flag ? 2 : 12, thin.pullback(2))
  }
}

// MARK: - Indirect Never result

// CHECK-LABEL:     // specialized pullback of branchGenericNever(_:_:)
// CHECK:           sil private @$s3src18branchGenericNeveryS2f_SbtFTJpSUpSr00181_43$s3src18branchGenericNeveryS2f_SbtFTJrSUpSr$s3src052_AD__$s3src18branchGenericNeveryS2f_SbtF_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_f02ab90f55e7612ffa3dfc731136fa08OD_wJHHHeuzfEeTf1nb_n :
// CHECK-NOT:       } // end sil function
// CHECK:           bb[[#]](%[[#PAYLOAD:]] : $(predecessor: _AD__$s3src18branchGenericNeveryS2f_SbtF_bb1__Pred__src_0_wrt_0, (_: Float))):
// CHECK-NEXT:        %[[#CAPTURES:]] = tuple_extract %[[#PAYLOAD]], 1
// CHECK-NEXT:        %[[#CAPTURE:]] = tuple_extract %[[#CAPTURES]], 0
// CHECK:             %[[#X:]] = struct_extract %[[#CAPTURE]], #Float._value
// CHECK-NEXT:        %[[#]] = builtin "fmul_FPIEEE32"(%[[#]], %[[#X]])
// CHECK:           } // end sil function

// Instantiating the generic error type with Never leaves an indirect error
// result, so the nonthrowing pullback needs a reabstraction thunk.
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

tests.test("NonthrowingAdapter") {
  for flag in [true, false] {
    let result = valueWithPullback(at: Float(3), of: {
      branchGenericNever($0, flag)
    })
    expectEqual(flag ? 9 : 6, result.value)
    for seed in [Float(1), 5, -2] {
      expectEqual((flag ? 6 : 2) * seed, result.pullback(seed))
    }
  }
}

// The outer closure has a static entry point and captures the runtime wrapper
// and the inner pullback.

public typealias Pullback = (Float) -> Float
// MARK: - Runtime pullback wrapper

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

tests.test("RuntimeWrapper") {
  for flag in [false, true] {

    var calls = 0
    let wrapper: Wrapper = { seed, pullback in
      calls += 1
      return pullback(seed)
    }
    let (value, pullback) = valueWithPullback(at: Float(3), of: {
      branchDynamic($0, "a label longer than a small string", flag, wrapper)
    })
    expectEqual(flag ? 6 : 9, value)
    expectEqual(flag ? 2 : 6, pullback(1))
    expectEqual(flag ? 4 : 12, pullback(2))
    expectEqual(flag ? 2 : 0, calls)

  }
}

runAllTests()
