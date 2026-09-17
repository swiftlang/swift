// RUN: %target-swift-frontend -emit-sil %s -O -o %t/out.sil
// RUN: cat %t/out.sil | %FileCheck %s

import _Differentiation

extension Array.DifferentiableView:
    @retroactive Sequence,
    @retroactive Collection,
    @retroactive RangeReplaceableCollection,
    @retroactive RandomAccessCollection
    where Element: Differentiable
{
    public typealias Element = Array.Element
    public typealias Index = Array.Index
    public typealias SubSequence = Array.SubSequence

    @inlinable
    public subscript(position: Index) -> Element {
        _read { yield base[position] }
        set(newValue) { base[position] = newValue }
    }

    @inlinable
    public subscript(bounds: Range<Index>) -> SubSequence {
        get { base[bounds] }
        set(newValue) { base[bounds] = newValue }
    }

    @inlinable
    public var startIndex: Index { base.startIndex }

    @inlinable
    public var endIndex: Index { base.endIndex }

    @inlinable
    public init() {
        self.init(Array<Element>())
    }

    @inlinable
    public mutating func replaceSubrange<C>(_ subrange: Range<Index>, with newElements: C)
        where C: Collection, Element == C.Element
    {
        base.replaceSubrange(subrange, with: newElements)
    }
}

public protocol DifferentiableCollection: Differentiable & Collection where
    Element: Differentiable,
    TangentVector: DifferentiableCollectionTangentVector,
    TangentVector.Element == Element.TangentVector
{
    associatedtype Element
    associatedtype TangentVector

    var tangentCount: Int { get }

    func tangentIndex(for i: Index) -> TangentVector.Index
}

extension DifferentiableCollection where Index == TangentVector.Index {
    @inlinable public var tangentCount: Int { count }
    @inlinable public func tangentIndex(for i: Index) -> TangentVector.Index { i }
}

public protocol DifferentiableCollectionTangentVector: DifferentiableCollection {
    init()
    init(repeating value: Element, count: Int)
    mutating func reserveCapacity(_ capacity: Int)
    mutating func writeTangentContribution(of value: Element, at index: Index)
}

extension Array: DifferentiableCollection where Element: Differentiable & AdditiveArithmetic {}

extension Array.DifferentiableView: DifferentiableCollection where Element: AdditiveArithmetic {}

extension Array.DifferentiableView: DifferentiableCollectionTangentVector where Element: AdditiveArithmetic {
    @inlinable public mutating func writeTangentContribution(of value: Element, at index: Index) {
        self[index] += value
    }
}

@inline(never)
@differentiable(reverse)
public func fusedScalarZip<C1>(
    _ c1: C1,
    with transform: @differentiable(reverse) (Double) -> Double
) -> [Double] where
    C1: DifferentiableCollection, C1.Element == Double
{
    let n = c1.count
    if n == 0 { return [] }
    var out = [Double](repeating: 0, count: n)
    var i1 = c1.startIndex
    for k in 0 ..< n {
        out[k] = transform(c1[i1])
        c1.formIndex(after: &i1)
    }
    return out
}

@inline(never)
func valueWithPullbackWrapper(at: Double, of: @differentiable(reverse) (Double) -> Double) -> (Double, (Double) -> Double) {
    return valueWithPullback(at: at, of: of)
}

@inline(never)
@derivative(of: fusedScalarZip)
public func _vjpFusedScalarZip<C1>(
    _ c1: C1,
    with transform: @differentiable(reverse) (Double) -> Double
) -> (
    value: [Double],
    pullback: ([Double].TangentVector) -> (C1.TangentVector)
) where
    C1: DifferentiableCollection, C1.Element == Double
{
    let n = c1.count
    let tangentCount1 = c1.tangentCount

    var out = [Double](repeating: 0, count: n)
    var partials1 = [Double](repeating: 0, count: n)
    var tangentIndices1 = [C1.TangentVector.Index]()
    tangentIndices1.reserveCapacity(n)

    var i1 = c1.startIndex
    for k in 0 ..< n {
        let (value, pullback) = valueWithPullbackWrapper(at: c1[i1], of: transform)
        out[k] = value
        let (d1) = pullback(1.0)
        partials1[k] = d1
        tangentIndices1.append(c1.tangentIndex(for: i1))
        c1.formIndex(after: &i1)
    }

    return (
        value: out,
        pullback: { v in
            var results1 = C1.TangentVector(repeating: .zero, count: tangentCount1)
            guard v.count != 0 else { return (results1) }
            precondition(v.count == n)
            var vi = v.startIndex
            for k in 0 ..< n {
                let dOut = v[vi]
                results1.writeTangentContribution(of: dOut * partials1[k], at: tangentIndices1[k])
                v.formIndex(after: &vi)
            }
            return (results1)
        }
    )
}

@differentiable(reverse)
public func caller(_ arr: [Double]) -> [Double] {
    return fusedScalarZip(arr) { 37 * $0 }
}


// CHECK: // specialized valueWithPullbackWrapper(at:of:)
// CHECK: sil shared [noinline] @$s3out24valueWithPullbackWrapper2at2ofSd_S2dctSd_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n30$s3out6callerySaySdGACFS2dcfU_0hijK9U_TJfSpSr0hijk5U_TJrmN0Tf1nccc_n : $@convention(thin) (Double) -> (Double, @owned @callee_guaranteed (Double) -> Double) {
// CHECK: bb0(%0 : $Double):
// CHECK:   %[[#A4:]] = float_literal $Builtin.FPIEEE64, 0x4042800000000000
// CHECK:   %[[#A5:]] = struct $Double (%[[#A4]])
// CHECK:   %[[#A6:]] = struct_extract %0, #Double._value
// CHECK:   %[[#A7:]] = builtin "fmul_FPIEEE64"(%[[#A4]], %[[#A6]]) : $Builtin.FPIEEE64
// CHECK:   %[[#A8:]] = struct $Double (%[[#A7]])
// CHECK:   // function_ref specialized pullback of closure #1 in caller(_:)
// CHECK:   %[[#A9:]] = function_ref @$s3out6callerySaySdGACFS2dcfU_TJpSpSr073$sSd16_DifferentiationE12_vjpMultiply3lhs3rhsSd5value_Sd_SdtSdc8pullbacktj1_k5FZSd_K6SdcfU_S2dTf1nE_n : $@convention(thin) (Double, Double, Double) -> Double
// CHECK:   %[[#A10:]] = partial_apply [callee_guaranteed] %[[#A9]](%0, %[[#A5]]) : $@convention(thin) (Double, Double, Double) -> Double
// CHECK:   %[[#A11:]] = tuple (%[[#A8]], %[[#A10]])
// CHECK:   return %[[#A11]]
// CHECK: } // end sil function '$s3out24valueWithPullbackWrapper2at2ofSd_S2dctSd_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n30$s3out6callerySaySdGACFS2dcfU_0hijK9U_TJfSpSr0hijk5U_TJrmN0Tf1nccc_n'


// CHECK:     // specialized _vjpFusedScalarZip<A>(_:with:)
// CHECK:     sil shared [noinline] @$s3out18_vjpFusedScalarZip_4withSaySdG5value_13TangentVectorQzSa16_DifferentiationAH14DifferentiableRzlE0K4ViewVySd_Gc8pullbacktx_S2dYjrXEtAA0K10CollectionRzSd7ElementRtzlFAD_Tg5S2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n30$s3out6callerySaySdGACFS2dcfU_0pqrS9U_TJfSpSr0pqrs5U_TJruV0Tf1nccc_n : $@convention(thin) (@guaranteed Array<Double>) -> (@owned Array<Double>, @owned @callee_guaranteed @substituted <τ_0_0> (@guaranteed Array<Double>.DifferentiableView) -> @out τ_0_0 for <Array<Double>.DifferentiableView>) {
// CHECK-NOT:   differentiable_function
// CHECK:       // function_ref specialized valueWithPullbackWrapper(at:of:)
// CHECK:       %[[#B127:]] = function_ref @$s3out24valueWithPullbackWrapper2at2ofSd_S2dctSd_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n30$s3out6callerySaySdGACFS2dcfU_0hijK9U_TJfSpSr0hijk5U_TJrmN0Tf1nccc_n : $@convention(thin) (Double) -> (Double, @owned @callee_guaranteed (Double) -> Double)
// CHECK:       %[[#B128:]] = apply %[[#B127]](%[[#]]) : $@convention(thin) (Double) -> (Double, @owned @callee_guaranteed (Double) -> Double)
// CHECK-NOT:   differentiable_function
// CHECK:     } // end sil function '$s3out18_vjpFusedScalarZip_4withSaySdG5value_13TangentVectorQzSa16_DifferentiationAH14DifferentiableRzlE0K4ViewVySd_Gc8pullbacktx_S2dYjrXEtAA0K10CollectionRzSd7ElementRtzlFAD_Tg5S2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n30$s3out6callerySaySdGACFS2dcfU_0pqrS9U_TJfSpSr0pqrs5U_TJruV0Tf1nccc_n'


// CHECK: // reverse-mode derivative of caller(_:)
// CHECK: sil @$s3out6callerySaySdGACFTJrSpSr : $@convention(thin) (@guaranteed Array<Double>) -> (@owned Array<Double>, @owned @callee_guaranteed (@guaranteed Array<Double>.DifferentiableView) -> @owned Array<Double>.DifferentiableView) {
// CHECK: bb0(%0 : $Array<Double>):
// CHECK:   // function_ref specialized _vjpFusedScalarZip<A>(_:with:)
// CHECK:   %[[#C2:]] = function_ref @$s3out18_vjpFusedScalarZip_4withSaySdG5value_13TangentVectorQzSa16_DifferentiationAH14DifferentiableRzlE0K4ViewVySd_Gc8pullbacktx_S2dYjrXEtAA0K10CollectionRzSd7ElementRtzlFAD_Tg5S2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n30$s3out6callerySaySdGACFS2dcfU_0pqrS9U_TJfSpSr0pqrs5U_TJruV0Tf1nccc_n : $@convention(thin) (@guaranteed Array<Double>) -> (@owned Array<Double>, @owned @callee_guaranteed @substituted <τ_0_0> (@guaranteed Array<Double>.DifferentiableView) -> @out τ_0_0 for <Array<Double>.DifferentiableView>)
// CHECK:   %[[#C3:]] = apply %[[#C2]](%0) : $@convention(thin) (@guaranteed Array<Double>) -> (@owned Array<Double>, @owned @callee_guaranteed @substituted <τ_0_0> (@guaranteed Array<Double>.DifferentiableView) -> @out τ_0_0 for <Array<Double>.DifferentiableView>)
// CHECK:   %[[#C4:]] = unchecked_bitwise_cast %[[#C3]] to $(Array<Double>, @callee_guaranteed (@guaranteed Array<Double>.DifferentiableView) -> @out Array<Double>.DifferentiableView)
// CHECK:   %[[#C5:]] = tuple_extract %[[#C4]], 0
// CHECK:   %[[#C6:]] = tuple_extract %[[#C4]], 1
// CHECK:   // function_ref specialized pullback of caller(_:)
// CHECK:   %[[#C7:]] = function_ref @$s3out6callerySaySdGACFTJpSpSr76$sSa16_DifferentiationAA14DifferentiableRzlE0B4ViewVySd_GAEIeggr_A2EIeggo_TRSa01_E0AE0G0RzlE0gK0VySd_GAIIeggr_Tf1nE_n : $@convention(thin) (@guaranteed Array<Double>.DifferentiableView, @owned @callee_guaranteed (@guaranteed Array<Double>.DifferentiableView) -> @out Array<Double>.DifferentiableView) -> @owned Array<Double>.DifferentiableView
// CHECK:   %[[#C8:]] = partial_apply [callee_guaranteed] %[[#C7]](%[[#C6]]) : $@convention(thin) (@guaranteed Array<Double>.DifferentiableView, @owned @callee_guaranteed (@guaranteed Array<Double>.DifferentiableView) -> @out Array<Double>.DifferentiableView) -> @owned Array<Double>.DifferentiableView
// CHECK:   %[[#C9:]] = tuple (%[[#C5]], %[[#C8]])
// CHECK:   return %[[#C9]]
// CHECK: } // end sil function '$s3out6callerySaySdGACFTJrSpSr'
