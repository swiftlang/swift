// RUN: %target-run-simple-swift(-Onone -swift-version 5 -parse-as-library -whole-module-optimization -Xfrontend -disable-availability-checking -Xfrontend -sil-verify-all %S/Inputs/async_vjp_values.swift %S/Inputs/async_vjp_expected.swift %S/Inputs/async_vjp_generic_cases.swift)
// RUN: %target-run-simple-swift(-O -swift-version 5 -parse-as-library -whole-module-optimization -Xfrontend -disable-availability-checking -Xfrontend -sil-verify-all %S/Inputs/async_vjp_values.swift %S/Inputs/async_vjp_expected.swift %S/Inputs/async_vjp_generic_cases.swift)
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: differentiable_programming

import _Differentiation
import StdlibUnittest

func registerGenericTests<S: BinaryFloatingPoint & Differentiable & Sendable>(
  _ type: S.Type, name: String
) where S == S.TangentVector {
  let tests = TestSuite("AsyncVJP/" + name)

  tests.test("PlainIdentity") {
    for x: S in [3, -2, 0] {
      checkScalarUnary(await valueWithPullback(at: x, of: plainGeneric),
                       value: x, derivative: 1)
    }
  }

  tests.test("TaskSquare") {
    for x: S in [3, -2, 0] {
      checkScalarUnary(await valueWithPullback(at: x, of: squareGeneric),
                       value: x * x, derivative: 2 * x)
    }
  }

  tests.test("NotUseful") {
    checkScalarUnary(await valueWithPullback(at: S(3), of: notUsefulGeneric),
                     value: 0, derivative: 0)
  }

  tests.test("NotVaried") {
    let z = S(7)
    checkScalarUnary(await valueWithPullback(at: S(3), of: { x in
      await notVariedGeneric(x, z)
    }), value: z, derivative: 0)
  }

  tests.test("ThreeChildren/ReorderedCaptures") {
    for row in threeChildExpectations {
      checkScalarBinary(await valueWithPullback(
        at: S(row.a), S(row.b), of: doWorkGeneric), row)
    }
  }

  tests.test("SynchronousControl") {
    for row in threeChildExpectations {
      checkScalarBinary(valueWithPullback(
        at: S(row.a), S(row.b), of: doWorkSyncGeneric), row)
    }
  }

  tests.test("Branches/BothArms") {
    for entry in branchExpectations {
      let flag = entry.flag
      let row = entry.result
      checkScalarBinary(await valueWithPullback(
        at: S(row.a), S(row.b), of: { a, b in
          await cfgGeneric(a, b, flag)
        }), row)
    }
  }

  tests.test("Loops/Inclusive/PerIterationPullbacks") {
    for entry in loopExpectations {
      let flag = entry.flag
      let n = entry.n
      for row in [entry.first, entry.second] {
        checkScalarBinary(await valueWithPullback(
          at: S(row.a), S(row.b), of: { a, b in
            await loopGeneric(a, b, flag, n)
          }), row)
      }
    }
  }

  tests.test("Loops/UnusedThirdWorkloadArgument") {
    for entry in loopExpectations {
      let flag = entry.flag
      let n = entry.n
      for row in [entry.first, entry.second] {
        checkScalarBinary(await valueWithPullback(
          at: S(row.a), S(row.b), of: { a, b in
            await ternaryGeneric(a, b, flag, n)
          }), row)
      }
    }
  }

  tests.test("ThreeDistinctCaptures/ZeroThirdGradient") {
    let (value, pullback) = await valueWithPullback(
      at: S(3), S(5), S(19), of: threeCapturesGeneric)
    expectEqual(S(15), value)
    for seed: S in [1, 2, 0, -1, 1] {
      let (da, db, dc) = pullback(seed)
      expectEqual(5 * seed, da)
      expectEqual(3 * seed, db)
      expectEqual(S(0), dc)
    }
  }

  tests.test("TaskIdentity") {
    checkScalarUnary(await valueWithPullback(at: S(3), of: identityChildGeneric),
                     value: 3, derivative: 1)
  }

  #if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)
  tests.test("InactiveBlockingWork") {
    for row in sleepExpectations {
      checkScalarBinary(await valueWithPullback(
        at: S(row.a), S(row.b), of: sleepWorkGeneric), row)
    }
  }
  #endif
}

// Non-generic call sites are intentional: these exercise the conversion of a
// specialized generic function value before it reaches the public async operator.
func registerGenericConversionTests() {
  let tests = TestSuite("AsyncVJP/GenericConversion")
  tests.test("Float") {
    for row in threeChildExpectations {
      checkScalarBinary(await valueWithPullback(
        at: Float(row.a), Float(row.b), of: doWorkGeneric), row)
    }
  }
  tests.test("Double") {
    for row in threeChildExpectations {
      checkScalarBinary(await valueWithPullback(
        at: Double(row.a), Double(row.b), of: doWorkGeneric), row)
    }
  }
}

// These generic functions do NOT impose T == T.TangentVector. In particular,
// HeapPair.TangentVector and Array<Double>.TangentVector differ from the primal.
func registerGenericNontrivialTests() {
  let tests = TestSuite("AsyncVJP/GenericNontrivial")
  tests.test("PlainIdentity/DifferentTangentType") {
    checkHeapUnary(await valueWithPullback(
      at: HeapPair(3, -2), of: plainGeneric),
      value: [3, -2], derivative: [1, 1])
  }
  tests.test("TaskIdentity/DifferentTangentType") {
    checkHeapUnary(await valueWithPullback(
      at: HeapPair(3, -2), of: identityChildGeneric),
      value: [3, -2], derivative: [1, 1])
  }
  tests.test("NotUseful") {
    checkHeapUnary(await valueWithPullback(
      at: HeapPair(3, -2), of: notUsefulGeneric),
      value: [0, 0], derivative: [0, 0])
  }
  tests.test("NotVaried") {
    let z = HeapPair(7, 11)
    checkHeapUnary(await valueWithPullback(at: HeapPair(3, -2), of: { x in
      await notVariedGeneric(x, z)
    }), value: [7, 11], derivative: [0, 0])
  }
  tests.test("ArrayResult/AddressOnly/ReusablePullback") {
    var input = [3.0, 5.0]
    let (value, pullback) = await valueWithPullback(
      at: input, of: identityChildGeneric)
    input[0] = 100
    expectEqual([3, 5], value)
    expectEqual([100, 5], input)
    for seed in heapSeeds {
      let gradient = pullback(Array<Double>.TangentVector(seed))
      expectEqual(seed, gradient.base)
    }
  }
}

@main
struct Main {
  static func main() async {
    registerGenericTests(Float.self, name: "Generic/Float")
    registerGenericTests(Double.self, name: "Generic/Double")
    registerGenericConversionTests()
    registerGenericNontrivialTests()
    await runAllTestsAsync()
  }
}
