// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name src %s -o %t.sil
// RUN: %FileCheck %s < %t.sil
// RUN: %FileCheck %s --check-prefix=TAPE < %t.sil
// RUN: %FileCheck %s --check-prefix=UNCHANGED < %t.sil

import _Differentiation

// MARK: - Distinct tapes for bound generic arguments

// TAPE-LABEL:      {{^}}enum _AD__$s3src6branchyS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_a30b5f1f54bb7acaed10f6b14037924a {
// TAPE:              case bb1((predecessor: _AD__$s3src6branchyS2f_xmtlF_bb1__Pred__src_0_wrt_0_l<Int>, (Float, Float)))
// TAPE-LABEL:      {{^}}}

// TAPE-LABEL:      {{^}}enum _AD__$s3src6branchyS2f_xmtlF_bb3__Pred__src_0_wrt_0_l_spec_bb2_1_bb1_1_68b51539a92d70661275c016eae0a581 {
// TAPE:              case bb1((predecessor: _AD__$s3src6branchyS2f_xmtlF_bb1__Pred__src_0_wrt_0_l<String>, (Float, Float)))
// TAPE-LABEL:      {{^}}}

// The VJPs are already concrete, but their tape declaration still has a
// phantom generic parameter. Int and String require distinct specialized tapes.

@differentiable(reverse, wrt: x)
@inline(never)
func branch<T>(_ x: Float, _ type: T.Type) -> Float {
  if x > 0 { return x * x }
  return x + x
}

public func intGradient(_ x: Float) -> Float {
  gradient(at: x, of: { branch($0, Int.self) })
}

public func stringGradient(_ x: Float) -> Float {
  gradient(at: x, of: { branch($0, String.self) })
}

// MARK: - Nested tapes with a captureless inner pullback

// TAPE-LABEL:      {{^}}enum _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l_spec_bb4_1_bb3_1_2_55fc03bc2eb20011bfdbef21472cd37e {
// TAPE:              case bb3((predecessor: _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb3__Pred__src_0_wrt_0_l<String>, (Float, Float), (_: _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l<Int>)))
// TAPE-LABEL:      {{^}}}

// TAPE-LABEL:      {{^}}enum _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l_spec_bb4_1_bb3_1_2_bdf3891784cbbba98ceb8f441e10a27c {
// TAPE:              case bb3((predecessor: _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb3__Pred__src_0_wrt_0_l<Double>, (Float, Float), (_: _AD__$s3src10nestedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l<Int>)))
// TAPE-LABEL:      {{^}}}

// Early inlining nests the Int branch tape inside the String and Double
// VJPs. ADCS must preserve its captureless callback and enum type.
@differentiable(reverse, wrt: x)
@inline(never)
public func nestedTape<T>(_ x: Float, _ type: T.Type, _ flag: Bool) -> Float {
  if T.self == Int.self { return x + x }
  if flag { return nestedTape(x * x, Int.self, false) }
  return x * x
}

@differentiable(reverse, wrt: x)
@inline(never)
public func stringNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, String.self, flag)
}

@differentiable(reverse, wrt: x)
@inline(never)
public func doubleNestedTape(_ x: Float, _ flag: Bool) -> Float {
  nestedTape(x, Double.self, flag)
}

// MARK: - Outer and inner tapes from separate instantiations of one primal

// TAPE-LABEL:      {{^}}enum _AD__$s3src10scopedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l_spec_bb4_1_bb3_1_2_a0bc93bf79843f84bcb2655473c9058d {
// TAPE:              case bb3((predecessor: _AD__$s3src10scopedTapeyS2f_xmSbtlF_bb3__Pred__src_0_wrt_0_l<String>, (Float, Float), (_: _AD__$s3src10scopedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l<Int>)))
// TAPE-LABEL:      {{^}}}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of stringScopedTape(_:_:)
// CHECK:             %[[#INT_TAPE:]] = enum $_AD__$s3src10scopedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l<Int>, #_AD__$s3src10scopedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l.bb1!enumelt,
// CHECK:             // function_ref specialized pullback of scopedTape<A>(_:_:_:)
// CHECK-NEXT:        %[[#INNER:]] = function_ref @$s3src10scopedTapeyS2f_xmSbtlFlTJpSUUpSrSi_Tg5 : $@convention(thin) (Float, @owned _AD__$s3src10scopedTapeyS2f_xmSbtlF_bb5__Pred__src_0_wrt_0_l<Int>) -> Float
// CHECK-NEXT:        partial_apply [callee_guaranteed] %[[#INNER]](%[[#INT_TAPE]])
// CHECK:           } // end sil function

// Both tapes come from the same primal. Specializing the String VJP's
// outer tape must leave the inlined Int tape intact.

@differentiable(reverse, wrt: x)
@inline(__always)
public func scopedTape<T>(_ x: Float, _ type: T.Type, _ flag: Bool) -> Float {
  if T.self == Int.self { return x * x }
  if flag { return scopedTape(x * x, Int.self, false) }
  return x * x
}

@differentiable(reverse, wrt: x)
@inline(never)
public func stringScopedTape(_ x: Float, _ flag: Bool) -> Float {
  scopedTape(x, String.self, flag)
}

// MARK: - Naming a tape from accessors and closures

// CHECK-LABEL:     {{^}}// reverse-mode derivative of GetterCase.value.getter
// CHECK-NOT:         = partial_apply
// CHECK:             // function_ref specialized pullback of GetterCase.value.getter
// CHECK-NEXT:        %[[#GETTER_FN:]] = function_ref @$s3src10GetterCaseV5valueSfvgTJpSpSr00168_36$s3src10GetterCaseV5valueSfvgTJrSpSr$s3src046_AD__$s3src10GetterCaseV5valueSfvg_bb3__Pred__A58_0_wrt_0_spec_bb2_1_bb1_1_1881bf8b54db9bdadfe01c3b0bd14d48OD_eHBJBeqlfreTf1nb_n :
// CHECK-NEXT:        %[[#GETTER_PB:]] = partial_apply [callee_guaranteed] %[[#GETTER_FN]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3src10GetterCaseV5valueSfvg_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_1881bf8b54db9bdadfe01c3b0bd14d48) -> GetterCase.TangentVector
// CHECK-NEXT:        %[[#GETTER_RESULT:]] = tuple (%[[#]], %[[#GETTER_PB]])
// CHECK-NEXT:        return %[[#GETTER_RESULT]]
// CHECK-NEXT:      } // end sil function

public struct GetterCase: Differentiable {
  public var x: Float

  public init(_ x: Float) { self.x = x }

  @differentiable(reverse)
  @inline(never)
  public var value: Float {
    if x > 0 { return x * x }
    return x + x
  }
}


public let branchClosure: @differentiable(reverse) (Float) -> Float = { x in
  if x > 0 { return x * x }
  return x + x
}

public func makeBranchClosure() -> @differentiable(reverse) (Float) -> Float {
  { x in
    if x > 0 { return x * x }
    return x + x
  }
}

public func getterGradient(_ x: Float) -> Float {
  gradient(at: x, of: { GetterCase($0).value })
}

public func closureGradient(_ x: Float) -> Float {
  gradient(at: x, of: branchClosure)
}

// MARK: - A predecessor marker in the generic signature

// CHECK-LABEL:     {{^}}// specialized pullback of branchWithMarker<A>(_:_:_:)
// CHECK:             switch_enum %[[#]], case #_AD__$s3src16branchWithMarkeryS2f_xSbtAA15P__Pred__SuffixRzlF_bb3__Pred__src_0_wrt_0_3src15P__Pred__SuffixRzl_spec_bb2_1_bb1_1_b77db1bda0cb02a01daf6e1dcc02caa3.bb2!enumelt:
// CHECK:           } // end sil function

// Put a second __Pred__ marker in the tape's generic signature.
public protocol P__Pred__Suffix {}
extension Int: P__Pred__Suffix {}

@inline(never)
@differentiable(reverse, wrt: x)
public func branchWithMarker<T: P__Pred__Suffix>(_ x: Float, _ t: T, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

@inline(never)
public func derivative<T: P__Pred__Suffix>(_ x: Float, _ t: T, _ flag: Bool) -> Float {
  gradient(at: x, of: { branchWithMarker($0, t, flag) })
}

precondition(derivative(2, 1, true) == 4)
precondition(derivative(2, 1, false) == 2)

// MARK: - Preserving a switch over an unrelated tangent enum

// CHECK-LABEL:     {{^}}// specialized pullback of branchOnTangent(_:_:)
// CHECK:             %[[#ZERO:]] = enum $Delta, #Delta.zero!enumelt
// CHECK:             switch_enum %[[#]], case #_AD__$s3src15branchOnTangentySfAA6ScalarV_SbtF_bb3__Pred__src_0_wrt_0_spec_bb1_1_0b66202f7c8a38c36d70e670d6fe7f54.bb2!enumelt: bb[[#]], case #_AD__$s3src15branchOnTangentySfAA6ScalarV_SbtF_bb3__Pred__src_0_wrt_0_spec_bb1_1_0b66202f7c8a38c36d70e670d6fe7f54.bb1!enumelt: bb[[#]]
// CHECK:             br [[JOIN:bb[0-9]+]](%[[#ZERO]])
// CHECK:           [[JOIN]](%[[#RESULT:]] : $Delta):
// CHECK:             return %[[#RESULT]]
// CHECK:             %[[#TANGENT:]] = struct $Float
// CHECK-NEXT:        %[[#VALUE:]] = enum $Delta, #Delta.value!enumelt, %[[#TANGENT]]
// CHECK:             br [[JOIN]](%[[#VALUE]])
// CHECK-NEXT:      } // end sil function

public enum Delta: Differentiable, AdditiveArithmetic {
  public typealias TangentVector = Self
  case zero, one, value(Float)

  var scalar: Float {
    switch self {
    case .zero: return 0
    case .one: return 1
    case let .value(x): return x
    }
  }

  @inline(__always)
  public static func + (lhs: Self, rhs: Self) -> Self {
    switch lhs {
    case .zero: return rhs
    default: return .value(lhs.scalar + rhs.scalar)
    }
  }

  public static func - (lhs: Self, rhs: Self) -> Self { .value(lhs.scalar - rhs.scalar) }
  public static func == (lhs: Self, rhs: Self) -> Bool { lhs.scalar == rhs.scalar }
}

public struct Scalar: Differentiable {
  public var value: Float
  public typealias TangentVector = Delta
  public mutating func move(by offset: Delta) { value += offset.scalar }
}

@differentiable(reverse)
public func scalarValue(_ x: Scalar) -> Float { x.value }

@derivative(of: scalarValue)
@inline(__always)
public func vjpScalarValue(_ x: Scalar) -> (value: Float, pullback: (Float) -> Delta) {
  (x.value, { .value($0) })
}

@differentiable(reverse, wrt: x)
public func branchOnTangent(_ x: Scalar, _ flag: Bool) -> Float {
  if flag { return scalarValue(x) }
  return 0
}

// The tangent switch is unrelated to the branch tape. Its default edge must
// survive cloning the pullback.

// MARK: - Preserving coroutine successors

public struct Accumulator: Differentiable {
  public var value: Float
  public init(_ value: Float) { self.value = value }

  public var state: Float {
    get { value }
    set { value = newValue }
    @inline(never) _modify { yield &value }
  }
}

// The single-yield accessor exposes a partial_apply for its pullback.
@differentiable(reverse, wrt: x)
public func mutateAndBranch(_ x: Float, _ flag: Bool) -> Float {
  var model = Accumulator(x)
  model.state *= x
  if flag { return model.state * x }
  return model.state
}

struct Pair: Differentiable {
  var first: Float
  var second: Float
  @noDerivative var chooseFirst: Bool

  var selected: Float {
    get { chooseFirst ? first : second }
    @inline(never) _modify {
      if chooseFirst { yield &first } else { yield &second }
    }
  }
}

// The two-yield accessor produces its pullback dynamically from end_apply.
@differentiable(reverse, wrt: x)
public func mutateSelected(_ x: Float, _ chooseFirst: Bool, _ scale: Bool) -> Float {
  var pair = Pair(first: x, second: x, chooseFirst: chooseFirst)
  pair.selected *= x
  if scale { return pair.first * x }
  return pair.second
}

// MARK: - Rewriting only affected predecessor types

// The switch arms save captures at the merge. The switch predecessors and
// the inputs to the first nested merge keep their original tape types.
// UNCHANGED-NOT:   enum _AD__$s3src6sparseyS2f_SitF_bb{{[0-6]}}__Pred__src_0_wrt_0_spec_
// UNCHANGED-NOT:   enum _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb{{[0-2]}}__Pred__src_0_wrt_0_spec_

// TAPE-LABEL:      {{^}}enum _AD__$s3src6sparseyS2f_SitF_bb7__Pred__src_0_wrt_0_spec_bb5_1_2_bb3_1_2_bb1_1_2_e42f88db982fa5eec2155d30485dc6e1 {
// TAPE:              case bb5((predecessor: _AD__$s3src6sparseyS2f_SitF_bb5__Pred__src_0_wrt_0, (), (Float, Float)))
// TAPE-LABEL:      {{^}}}

// TAPE-LABEL:      {{^}}enum _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_6bb6fb315d1ebecae82d784469deeb5c {
// TAPE-LABEL:      {{^}}enum _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb5__Pred__src_0_wrt_0_spec_c8920357b2cf1e5913f51162908fb765 {
// TAPE-NEXT:         case bb3((predecessor: _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_6bb6fb315d1ebecae82d784469deeb5c))
// TAPE-LABEL:      {{^}}}
// TAPE-LABEL:      {{^}}enum _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb4__Pred__src_0_wrt_0_spec_97060b7ace2e3fad78508b5f82b139ef {
// TAPE-NEXT:         case bb3((predecessor: _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb3__Pred__src_0_wrt_0_spec_bb2_1_bb1_1_6bb6fb315d1ebecae82d784469deeb5c))
// TAPE-LABEL:      {{^}}}
// TAPE-LABEL:      {{^}}enum _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb6__Pred__src_0_wrt_0_spec_bb4_1_ee77304943f6a20940bdb77963890bb5 {
// TAPE-NEXT:         case bb5((predecessor: _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb5__Pred__src_0_wrt_0_spec_c8920357b2cf1e5913f51162908fb765))
// TAPE:              case bb4((predecessor: _AD__$s3src18nestedPredecessorsyS2f_S2btF_bb4__Pred__src_0_wrt_0_spec_97060b7ace2e3fad78508b5f82b139ef, (Float, Float)))
// TAPE-LABEL:      {{^}}}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of sparse(_:_:)
// CHECK:             // function_ref specialized pullback of sparse(_:_:)
// CHECK-NEXT:        %[[#SPARSE:]] = function_ref @$s3src6sparseyS2f_SitFTJpSUpSr00167_30$s3src6sparseyS2f_SitFTJrSUpSr$s3src039_AD__$s3src6sparseyS2f_SitF_bb7__Pred__A70_0_wrt_0_spec_bb5_1_2_bb3_1_2_bb1_1_2_e42f88db982fa5eec2155d30485dc6e1OD_iEGEBenkfqeTf1nb_n :
// CHECK-NEXT:        partial_apply [callee_guaranteed] %[[#SPARSE]](
// CHECK:           } // end sil function

@differentiable(reverse, wrt: x)
public func sparse(_ x: Float, _ tag: Int) -> Float {
  switch tag {
  case 0: return (x + 1) * x
  case 1: return (x + 2) * x
  case 2: return (x + 3) * x
  default: return x
  }
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of nestedPredecessors(_:_:_:)
// CHECK:             // function_ref specialized pullback of nestedPredecessors(_:_:_:)
// CHECK-NEXT:        %[[#NESTED:]] = function_ref @$s3src18nestedPredecessorsyS2f_S2btFTJpSUUpSr00178_45$s3src18nestedPredecessorsyS2f_S2btFTJrSUUpSr$s3src053_AD__$s3src18nestedPredecessorsyS2f_S2btF_bb6__Pred__A52_0_wrt_0_spec_bb4_1_ee77304943f6a20940bdb77963890bb5OD_IACEGelwfBeTf1nb_n :
// CHECK-NEXT:        partial_apply [callee_guaranteed] %[[#NESTED]](
// CHECK:           } // end sil function

// Both successors require a new type even though they only carry a predecessor.
@differentiable(reverse, wrt: x)
public func nestedPredecessors(_ x: Float, _ first: Bool, _ second: Bool) -> Float {
  let interim = first ? x * x : x + x
  return second ? interim * x : interim
}

// CHECK-LABEL:     {{^}}// reverse-mode derivative of closure #1 in makeBranchClosure()
// CHECK-NOT:         = partial_apply
// CHECK:             // function_ref specialized pullback of closure #1 in makeBranchClosure()
// CHECK-NEXT:        %[[#NESTED_FN:]] = function_ref @$s3src17makeBranchClosureS2fYjrcyFS2fcfU_TJpSpSr00192_48$s3src17makeBranchClosureS2fYjrcyFS2fcfU_TJrSpSr$s3src058_AD__$s3src17makeBranchClosureS2fYjrcyFS2fcfU__bb4__Pred__A58_0_wrt_0_spec_bb3_1_bb1_1_c054b168c1142375eb80fc2b64452cc5OD_nGDHCfsfggfTf1nb_n :
// CHECK-NEXT:        %[[#NESTED_PB:]] = partial_apply [callee_guaranteed] %[[#NESTED_FN]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3src17makeBranchClosureS2fYjrcyFS2fcfU__bb4__Pred__src_0_wrt_0_spec_bb3_1_bb1_1_c054b168c1142375eb80fc2b64452cc5) -> Float
// CHECK-NEXT:        %[[#NESTED_RESULT:]] = tuple (%[[#]], %[[#NESTED_PB]])
// CHECK-NEXT:        return %[[#NESTED_RESULT]]
// CHECK-NEXT:      } // end sil function

// The reverse-mode derivative of the global branchClosure.
// CHECK-LABEL:     {{^}}// reverse-mode derivative of closure #1 in{{ *$}}
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil private @$s3srcS2fcfU_TJrSpSr :
// CHECK-NOT:         = partial_apply
// CHECK:             // function_ref specialized pullback of closure #1 in
// CHECK-NEXT:        %[[#GLOBAL_FN:]] = function_ref @$s3srcS2fcfU_TJpSpSr00136_20$s3srcS2fcfU_TJrSpSr$s3src030_AD__$s3srcS2fcfU__bb4__Pred__A58_0_wrt_0_spec_bb3_1_bb1_1_a6799eebfd75b3ceaa91dacb92ca0b34OD_BkJEHczmeudTf1nb_n :
// CHECK-NEXT:        %[[#GLOBAL_PB:]] = partial_apply [callee_guaranteed] %[[#GLOBAL_FN]](%[[#]]) : $@convention(thin) (Float, @owned _AD__$s3srcS2fcfU__bb4__Pred__src_0_wrt_0_spec_bb3_1_bb1_1_a6799eebfd75b3ceaa91dacb92ca0b34) -> Float
// CHECK-NEXT:        %[[#GLOBAL_RESULT:]] = tuple (%[[#]], %[[#GLOBAL_PB]])
// CHECK-NEXT:        return %[[#GLOBAL_RESULT]]
// CHECK-NEXT:      } // end sil function

// The optimized pullbacks keep a paired coroutine access around the two
// multiplications of the saved values.
// CHECK-LABEL:     {{^}}// specialized pullback of mutateAndBranch(_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil private @$s3src15mutateAndBranchyS2f_SbtFTJpSUpSr00177_40$s3src15mutateAndBranchyS2f_SbtFTJrSUpSr$s3src049_AD__$s3src15mutateAndBranchyS2f_SbtF_bb3__Pred__A60_0_wrt_0_spec_bb2_1_bb1_1_2_080100929f376e373738a1366fcdc5dcOD_dyHJFeivfAeTf1nb_nTf4ng_n :
// CHECK:             %[[#ACCESS:]] = convert_function %[[#]] to $@yield_once @callee_guaranteed (@inout Accumulator.TangentVector) -> @yields @inout Float
// CHECK-NEXT:        (%[[#YIELD:]], %[[#TOKEN:]]) = begin_apply %[[#ACCESS]](
// CHECK:             builtin "fmul_FPIEEE32"
// CHECK:             builtin "fmul_FPIEEE32"
// CHECK:             store %[[#]] to %[[#YIELD]]
// CHECK:             end_apply %[[#TOKEN]] as $()
// CHECK:           } // end sil function

// CHECK-LABEL:     {{^}}// specialized pullback of mutateSelected(_:_:_:)
// CHECK-NOT:       {{^}}sil
// CHECK:           {{^}}sil private @$s3src14mutateSelectedyS2f_S2btFTJpSUUpSr00170_41$s3src14mutateSelectedyS2f_S2btFTJrSUUpSr$s3src049_AD__$s3src14mutateSelectedyS2f_S2btF_bb3__Pred__A52_0_wrt_0_spec_bb1_1_5ca0adc489fb926bd1a67f27c81b4c1eOD_FdCICewnfteTf1nb_nTf4ng_n :
// CHECK:             %[[#ACCESS:]] = convert_function %[[#]] to $@yield_once @callee_guaranteed (@inout Pair.TangentVector) -> @yields @inout Float
// CHECK-NEXT:        (%[[#YIELD:]], %[[#TOKEN:]]) = begin_apply %[[#ACCESS]](
// CHECK:             builtin "fmul_FPIEEE32"
// CHECK:             builtin "fmul_FPIEEE32"
// CHECK:             store %[[#]] to %[[#YIELD]]
// CHECK:             end_apply %[[#TOKEN]] as $()
// CHECK:           } // end sil function
