// REQUIRES: executable_test
// RUN: %target-run-simple-swift(-O)

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil %s -O -module-name=src > %t/out.sil
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK1
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK2
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK3
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK4
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK5
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK6
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK7
// RUN: cat %t/out.sil | %FileCheck %s --check-prefix=CHECK7DISTINCT

import _Differentiation
import StdlibUnittest

@inline(never)
@differentiable(reverse)
func square(_ x: Double) -> Double {
  x * x
}

public protocol Test1Protocol {
  func value() -> Double
}

// CHECK1: // test1Entry(_:)
// CHECK1: sil [signature_optimized_thunk] [heuristic_always_inline] @$s3src10test1EntryySdAA13Test1Protocol_pF : $@convention(thin) (@in_guaranteed any Test1Protocol) -> Double {
// CHECK1-NOT: differentiable_function
// CHECK1: } // end sil function '$s3src10test1EntryySdAA13Test1Protocol_pF'

// CHECK1: // specialized test1Entry(_:)
// CHECK1: sil shared @$s3src10test1EntryySdAA13Test1Protocol_pFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : Test1Protocol> (@in_guaranteed τ_0_0) -> Double {
// CHECK1-NOT: differentiable_function
// CHECK1: } // end sil function '$s3src10test1EntryySdAA13Test1Protocol_pFTf4e_n'

// CHECK1: // specialized test1WithSquare(_:)
// CHECK1: sil shared [noinline] @$s3src15test1WithSquareyS4dYjrXEXEF64$s3src10test1EntryySdAA13Test1Protocol_pF0B8EvaluateL_yS3dYjrXEFAA0iJ0_pTf1c_nTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : Test1Protocol> (@in_guaranteed τ_0_0) -> Double {
// CHECK1-NOT: differentiable_function
// CHECK1: } // end sil function '$s3src15test1WithSquareyS4dYjrXEXEF64$s3src10test1EntryySdAA13Test1Protocol_pF0B8EvaluateL_yS3dYjrXEFAA0iJ0_pTf1c_nTf4e_n'

// CHECK1: // specialized test1Evaluate #1 (_:) in test1Entry(_:)
// CHECK1: sil private [noinline] @$s3src10test1EntryySdAA13Test1Protocol_pF0B8EvaluateL_yS3dYjrXEFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1an_n18$s3src6squareyS2dF0gH8FTJfSpSr0gh4FTJrjK0Tf1nccc_nTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : Test1Protocol> (@in_guaranteed τ_0_0) -> Double {
// CHECK1-NOT: differentiable_function
// CHECK1: } // end sil function '$s3src10test1EntryySdAA13Test1Protocol_pF0B8EvaluateL_yS3dYjrXEFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1an_n18$s3src6squareyS2dF0gH8FTJfSpSr0gh4FTJrjK0Tf1nccc_nTf4e_n'

@inline(never)
func test1WithSquare(
  _ body: (@differentiable(reverse) (Double) -> Double) -> Double
) -> Double {
  body(square)
}

public func test1Entry(_ p: any Test1Protocol) -> Double {
  @inline(never)
  func test1Evaluate(
    _ function: @differentiable(reverse) (Double) -> Double
  ) -> Double {
    gradient(at: p.value(), of: function)
  }

  return test1WithSquare(test1Evaluate)
}

public class Test2Base {
  public let value: Double

  public init(_ value: Double) {
    self.value = value
  }
}

public class Test2Derived: Test2Base {}

@inline(never)
@differentiable(reverse, wrt: x)
func test2Evaluate(_ x: Double, _ context: Test2Base) -> Double {
  x * x + context.value
}

@inline(never)
func test2UseConverted(
  _ x: Double,
  _ context: Test2Derived,
  _ function: @differentiable(reverse)
    (Double, @noDerivative Test2Derived) -> Double
) -> Double {
  function(x, context)
}

// CHECK2: // test2Entry(_:_:)
// CHECK2: sil @$s3src10test2EntryyS2d_AA12Test2DerivedCtF : $@convention(thin) (Double, @guaranteed Test2Derived) -> Double {
// CHECK2-NOT: differentiable_function
// CHECK2: } // end sil function '$s3src10test2EntryyS2d_AA12Test2DerivedCtF'

// CHECK2: // test2Evaluate(_:_:)
// CHECK2: sil hidden [noinline] @$s3src13test2EvaluateyS2d_AA9Test2BaseCtF : $@convention(thin) (Double, @guaranteed Test2Base) -> Double {
// CHECK2-NOT: differentiable_function
// CHECK2: } // end sil function '$s3src13test2EvaluateyS2d_AA9Test2BaseCtF'

// CHECK2: // specialized test2UseConverted(_:_:_:)
// CHECK2: sil shared [noinline] @$s3src17test2UseConvertedyS2d_AA12Test2DerivedCS2d_ADYktYjrXEtFSdADSdIgygd_SdADS3dIegyd_Igygdo_SdADS3dIegyd_Igygdo_Tf1nna_n029$s3src13test2EvaluateyS2d_AA9E7BaseCtF0ghi1_jekL9FTJfSUpSr0ghi1_jekl4FTJrnO0Tf1nnccc_n : $@convention(thin) (Double, @guaranteed Test2Derived) -> Double {
// CHECK2-NOT: differentiable_function
// CHECK2: } // end sil function '$s3src17test2UseConvertedyS2d_AA12Test2DerivedCS2d_ADYktYjrXEtFSdADSdIgygd_SdADS3dIegyd_Igygdo_SdADS3dIegyd_Igygdo_Tf1nna_n029$s3src13test2EvaluateyS2d_AA9E7BaseCtF0ghi1_jekL9FTJfSUpSr0ghi1_jekl4FTJrnO0Tf1nnccc_n'

public func test2Entry(_ x: Double, _ context: Test2Derived) -> Double {
  test2UseConverted(x, context, test2Evaluate)
}

@inline(never)
func test3Evaluate(
  _ x: Double,
  _ function: @differentiable(reverse) (Double) -> Double
) -> Double {
  gradient(at: x, of: function)
}

@inline(never)
func test3Forward<T>(
  _ x: T,
  _ body: (T, @differentiable(reverse) (Double) -> Double) -> T
) -> T {
  body(x, square)
}

// CHECK3: // test3Entry(_:)
// CHECK3: sil @$s3src10test3EntryyS2dF : $@convention(thin) (Double) -> Double {
// CHECK3-NOT: differentiable_function
// CHECK3: } // end sil function '$s3src10test3EntryyS2dF'

// CHECK3: // specialized test3Evaluate(_:_:)
// CHECK3: sil shared [noinline] @$s3src13test3EvaluateyS2d_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0Tf1nccc_n : $@convention(thin) (Double) -> Double {
// CHECK3-NOT: differentiable_function
// CHECK3: } // end sil function '$s3src13test3EvaluateyS2d_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0Tf1nccc_n'

// CHECK3: // specialized test3Forward<A>(_:_:)
// CHECK3: sil shared [noinline] @$s3src12test3Forwardyxx_xx_S2dYjrXEtXEtlFSd_Tg5019$sS3dIrgyd_SdIgygd_ef1_G57Iegngr_TR026$s3src13test3EvaluateyS2d_C7YjrXEtFTf3nnnpf_nTf1nc_n : $@convention(thin) (Double) -> Double {
// CHECK3-NOT: differentiable_function
// CHECK3: } // end sil function '$s3src12test3Forwardyxx_xx_S2dYjrXEtXEtlFSd_Tg5019$sS3dIrgyd_SdIgygd_ef1_G57Iegngr_TR026$s3src13test3EvaluateyS2d_C7YjrXEtFTf3nnnpf_nTf1nc_n'

public func test3Entry(_ x: Double) -> Double {
  test3Forward(x, test3Evaluate)
}

@inline(never)
func test4Evaluate<T, R>(
  _ x: T,
  _ function: @differentiable(reverse) (T) -> R
) -> T.TangentVector
where
  T: Differentiable,
  R: Differentiable & FloatingPoint,
  R == R.TangentVector
{
  gradient(at: x, of: function)
}

@inline(__always)
func test4Forward<T, R>(
  _ x: T,
  _ function: @escaping @differentiable(reverse) (T) -> R
) -> T.TangentVector
where
  T: Differentiable,
  R: Differentiable & FloatingPoint,
  R == R.TangentVector
{
  test4Evaluate(x, function)
}

// CHECK4: // test4Entry(_:)
// CHECK4: sil @$s3src10test4EntryyS2dF : $@convention(thin) (Double) -> Double {
// CHECK4-NOT: differentiable_function
// CHECK4: } // end sil function '$s3src10test4EntryyS2dF'

// CHECK4: // specialized test4Evaluate<A, B>(_:_:)
// CHECK4: sil shared [noinline] @$s3src13test4Evaluatey13TangentVectorQzx_q_xYjrXEt16_Differentiation14DifferentiableRzSFR_AeFR_AceFPQy_Rs_r0_lFSd_SdTg5xq_Ri_zRi1_zRi0_zRi__Ri1__Ri0__r0_lyS2dIsegnr_xq_xq_Ri_zRi1_zRi0_zRi__Ri1__Ri0__r0_lyq0_q1_Isegnr_Ri_zRi1_zRi0_zRi__Ri1__Ri0__Ri_0_Ri1_0_Ri0_0_Ri_1_Ri1_1_Ri0_1_r2_lyS4dIsegnro_xq_xq_Ri_zRi1_zRi0_zRi__Ri1__Ri0__r0_lyq0_q1_Isegnr_Ri_zRi1_zRi0_zRi__Ri1__Ri0__Ri_0_Ri1_0_Ri0_0_Ri_1_Ri1_1_Ri0_1_r2_lyS4dIsegnro_Tf1na_n011$sS2dIegyd_i24Iegnr_TR18$s3src6squareyI10FTf3nnpf_n0h3S4dj28_Iegydo_S2dxq_Ri_zRi1_zRi0_zq2__r2__s7__r0_lyi32Isegnr_Iegnro_TR25$s3src6squareyI17FTJfSpSrTf3nnpf_n0hnj1_o1_p1_q2_zr2_zs2_zq2__r2__s2__t1_uiv1_w1_xi4FTJrZ11SrTf3nnpf_nTf1nccc_n : $@convention(thin) (Double) -> Double {
// CHECK4-NOT: differentiable_function
// CHECK4: } // end sil function '$s3src13test4Evaluatey13TangentVectorQzx_q_xYjrXEt16_Differentiation14DifferentiableRzSFR_AeFR_AceFPQy_Rs_r0_lFSd_SdTg5xq_Ri_zRi1_zRi0_zRi__Ri1__Ri0__r0_lyS2dIsegnr_xq_xq_Ri_zRi1_zRi0_zRi__Ri1__Ri0__r0_lyq0_q1_Isegnr_Ri_zRi1_zRi0_zRi__Ri1__Ri0__Ri_0_Ri1_0_Ri0_0_Ri_1_Ri1_1_Ri0_1_r2_lyS4dIsegnro_xq_xq_Ri_zRi1_zRi0_zRi__Ri1__Ri0__r0_lyq0_q1_Isegnr_Ri_zRi1_zRi0_zRi__Ri1__Ri0__Ri_0_Ri1_0_Ri0_0_Ri_1_Ri1_1_Ri0_1_r2_lyS4dIsegnro_Tf1na_n011$sS2dIegyd_i24Iegnr_TR18$s3src6squareyI10FTf3nnpf_n0h3S4dj28_Iegydo_S2dxq_Ri_zRi1_zRi0_zq2__r2__s7__r0_lyi32Isegnr_Iegnro_TR25$s3src6squareyI17FTJfSpSrTf3nnpf_n0hnj1_o1_p1_q2_zr2_zs2_zq2__r2__s2__t1_uiv1_w1_xi4FTJrZ11SrTf3nnpf_nTf1nccc_n'

public func test4Entry(_ x: Double) -> Double {
  test4Forward(x, square)
}

@inline(never)
@differentiable(reverse, wrt: x)
func test5Evaluate(_ x: Double, _ context: Test2Base) -> Double {
  (x * x + 1) * context.value
}

@inline(never)
func test5UseConvertedVJP(
  _ x: Double,
  _ context: Test2Derived,
  _ function: @differentiable(reverse)
    (Double, @noDerivative Test2Derived) -> Double
) -> (value: Double, gradient: Double) {
  valueWithGradient(at: x) { function($0, context) }
}

// CHECK5: // test5Entry(_:_:)
// CHECK5: sil @$s3src10test5EntryySd5value_Sd8gradienttSd_AA12Test2DerivedCtF : $@convention(thin) (Double, @guaranteed Test2Derived) -> (Double, Double) {
// CHECK5-NOT: differentiable_function
// CHECK5: } // end sil function '$s3src10test5EntryySd5value_Sd8gradienttSd_AA12Test2DerivedCtF'

// CHECK5: // test5Evaluate(_:_:)
// CHECK5: sil hidden [noinline] @$s3src13test5EvaluateyS2d_AA9Test2BaseCtF : $@convention(thin) (Double, @guaranteed Test2Base) -> Double {
// CHECK5-NOT: differentiable_function
// CHECK5: } // end sil function '$s3src13test5EvaluateyS2d_AA9Test2BaseCtF'

// CHECK5: // specialized test5UseConvertedVJP(_:_:_:)
// CHECK5: sil shared [noinline] @$s3src20test5UseConvertedVJPySd5value_Sd8gradienttSd_AA12Test2DerivedCS2d_AFYktYjrXEtFSdAFSdIgygd_SdAFS3dIegyd_Igygdo_SdAFS3dIegyd_Igygdo_Tf1nna_n029$s3src13test5EvaluateyS2d_AA9H7BaseCtF0jkl1_mhnO9FTJfSUpSr0jkl1_mhno4FTJrqR0Tf1nnccc_n : $@convention(thin) (Double, @guaranteed Test2Derived) -> (Double, Double) {
// CHECK5-NOT: differentiable_function
// CHECK5: } // end sil function '$s3src20test5UseConvertedVJPySd5value_Sd8gradienttSd_AA12Test2DerivedCS2d_AFYktYjrXEtFSdAFSdIgygd_SdAFS3dIegyd_Igygdo_SdAFS3dIegyd_Igygdo_Tf1nna_n029$s3src13test5EvaluateyS2d_AA9H7BaseCtF0jkl1_mhnO9FTJfSUpSr0jkl1_mhno4FTJrqR0Tf1nnccc_n'

public func test5Entry(
  _ x: Double, _ context: Test2Derived
) -> (value: Double, gradient: Double) {
  test5UseConvertedVJP(x, context, test5Evaluate)
}

@inline(never)
func test6Evaluate(
  _ x: Double,
  _ function: @differentiable(reverse) (Double) -> Double
) -> Double {
  gradient(at: x, of: function)
}

@inline(never)
func test6Forward(
  _ x: Double,
  _ function: @differentiable(reverse) (Double) -> Double
) -> Double {
  test6Evaluate(x, function) + test6Evaluate(x + 1, function)
}

// CHECK6: // test6Entry(_:)
// CHECK6: sil @$s3src10test6EntryyS2dF : $@convention(thin) (Double) -> Double {
// CHECK6-NOT: differentiable_function
// CHECK6: } // end sil function '$s3src10test6EntryyS2dF'

// CHECK6: // specialized test6Evaluate(_:_:)
// CHECK6: sil shared [noinline] @$s3src13test6EvaluateyS2d_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0Tf1nccc_n : $@convention(thin) (Double) -> Double {
// CHECK6-NOT: differentiable_function
// CHECK6: } // end sil function '$s3src13test6EvaluateyS2d_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0Tf1nccc_n'

// CHECK6: // specialized test6Forward(_:_:)
// CHECK6: sil shared [noinline] @$s3src12test6ForwardyS2d_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0Tf1nccc_n : $@convention(thin) (Double) -> Double {
// CHECK6-NOT: differentiable_function
// CHECK6: } // end sil function '$s3src12test6ForwardyS2d_S2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1na_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0Tf1nccc_n'

public func test6Entry(_ x: Double) -> Double {
  test6Forward(x, square)
}

@inline(never)
@differentiable(reverse)
func cube(_ x: Double) -> Double {
  x * x * x
}

@inline(never)
func test7Evaluate(
  _ x: Double,
  _ first: @differentiable(reverse) (Double) -> Double,
  _ second: @differentiable(reverse) (Double) -> Double
) -> Double {
  gradient(at: x, of: first) - 3 * gradient(at: x + 2, of: second)
}

// CHECK7: // test7Entry(_:)
// CHECK7: sil @$s3src10test7EntryyS2dF : $@convention(thin) (Double) -> Double {
// CHECK7-NOT: differentiable_function
// CHECK7: } // end sil function '$s3src10test7EntryyS2dF'

// CHECK7: // specialized test7Evaluate(_:_:_:)
// CHECK7: sil shared [noinline] @$s3src13test7EvaluateyS2d_S2dYjrXES2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_S2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1naa_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0AcdETf1ncccccc_n : $@convention(thin) (Double) -> Double {
// CHECK7-NOT: differentiable_function
// CHECK7: } // end sil function '$s3src13test7EvaluateyS2d_S2dYjrXES2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_S2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1naa_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0AcdETf1ncccccc_n'

public func test7Entry(_ x: Double) -> Double {
  return test7Evaluate(x, square, square)
}

// CHECK7DISTINCT: // test7DistinctEntry(_:)
// CHECK7DISTINCT: sil @$s3src18test7DistinctEntryyS2dF : $@convention(thin) (Double) -> Double {
// CHECK7DISTINCT-NOT: differentiable_function
// CHECK7DISTINCT: } // end sil function '$s3src18test7DistinctEntryyS2dF'

// CHECK7DISTINCT: // specialized test7Evaluate(_:_:_:)
// CHECK7DISTINCT: sil shared [noinline] @$s3src13test7EvaluateyS2d_S2dYjrXES2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_S2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1naa_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0012$s3src4cubeyE1F0jefgH00jeigH0Tf1ncccccc_n : $@convention(thin) (Double) -> Double {
// CHECK7DISTINCT-NOT: differentiable_function
// CHECK7DISTINCT: } // end sil function '$s3src13test7EvaluateyS2d_S2dYjrXES2dYjrXEtFS2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_S2dIgyd_S4dIegyd_Igydo_S4dIegyd_Igydo_Tf1naa_n18$s3src6squareyS2dF0dE8FTJfSpSr0de4FTJrgH0012$s3src4cubeyE1F0jefgH00jeigH0Tf1ncccccc_n'

public func test7DistinctEntry(_ x: Double) -> Double {
  test7Evaluate(x, square, cube)
}


private struct Test1Value: Test1Protocol {
  let x: Double

  func value() -> Double { x }
}

var tests = TestSuite("DifferentiableFunctionSpecialization")

tests.test("Test1") {
  for x in [-3.0, 0.0, 2.0, 5.0] {
    expectEqual(2 * x, test1Entry(Test1Value(x: x)))
  }
}

tests.test("Test2") {
  for contextValue in [-2.0, 7.0] {
    let context = Test2Derived(contextValue)
    for x in [-3.0, 0.0, 2.0, 5.0] {
      expectEqual(x * x + contextValue, test2Entry(x, context))
    }
  }
}

tests.test("Test3") {
  for x in [-3.0, 0.0, 2.0, 5.0] {
    expectEqual(2 * x, test3Entry(x))
  }
}

tests.test("Test4") {
  for x in [-3.0, 0.0, 2.0, 5.0] {
    expectEqual(2 * x, test4Entry(x))
  }
}

tests.test("Test5") {
  for contextValue in [-2.0, 0.0, 3.0] {
    let context = Test2Derived(contextValue)
    for x in [-3.0, 0.0, 2.0, 5.0] {
      let result = test5Entry(x, context)
      expectEqual((x * x + 1) * contextValue, result.value)
      expectEqual(2 * x * contextValue, result.gradient)
    }
  }
}

tests.test("Test6") {
  for x in [-3.0, 0.0, 2.0, 5.0] {
    expectEqual(2 * x + 2 * (x + 1), test6Entry(x))
  }
}

tests.test("Test7") {
  for x in [-3.0, 0.0, 2.0, 5.0] {
    expectEqual(2 * x - 6 * (x + 2), test7Entry(x))
  }
}

tests.test("Test7Distinct") {
  for x in [-3.0, 0.0, 2.0, 5.0] {
    expectEqual(2 * x - 9 * (x + 2) * (x + 2), test7DistinctEntry(x))
  }
}

runAllTests()
