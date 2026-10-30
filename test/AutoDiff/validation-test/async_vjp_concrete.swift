// RUN: %target-run-simple-swift(-Onone -swift-version 5 -parse-as-library -whole-module-optimization -Xfrontend -disable-availability-checking -Xfrontend -sil-verify-all %S/Inputs/async_vjp_values.swift %S/Inputs/async_vjp_expected.swift %S/Inputs/async_vjp_concrete_cases.swift)
// RUN: %target-run-simple-swift(-O -swift-version 5 -parse-as-library -whole-module-optimization -Xfrontend -disable-availability-checking -Xfrontend -sil-verify-all %S/Inputs/async_vjp_values.swift %S/Inputs/async_vjp_expected.swift %S/Inputs/async_vjp_concrete_cases.swift)
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: differentiable_programming

import _Differentiation
import StdlibUnittest

func registerFloatTests() {
  let tests = TestSuite("AsyncVJP/" + "Float")

  tests.test("PlainIdentity") {
    for x: Float in [3, -2, 0] {
      checkScalarUnary(await valueWithPullback(at: x, of: plainFloat),
                       value: x, derivative: 1)
    }
  }

  tests.test("TaskSquare") {
    for x: Float in [3, -2, 0] {
      checkScalarUnary(await valueWithPullback(at: x, of: squareFloat),
                       value: x * x, derivative: 2 * x)
    }
  }

  tests.test("NotUseful") {
    checkScalarUnary(await valueWithPullback(at: Float(3), of: notUsefulFloat),
                     value: 0, derivative: 0)
  }

  tests.test("NotVaried") {
    let z = Float(7)
    checkScalarUnary(await valueWithPullback(at: Float(3), of: { x in
      await notVariedFloat(x, z)
    }), value: z, derivative: 0)
  }

  tests.test("ThreeChildren/ReorderedCaptures") {
    for row in threeChildExpectations {
      checkScalarBinary(await valueWithPullback(
        at: Float(row.a), Float(row.b), of: doWorkFloat), row)
    }
  }

  tests.test("SynchronousControl") {
    for row in threeChildExpectations {
      checkScalarBinary(valueWithPullback(
        at: Float(row.a), Float(row.b), of: doWorkSyncFloat), row)
    }
  }

  tests.test("Branches/BothArms") {
    for entry in branchExpectations {
      let flag = entry.flag
      let row = entry.result
      checkScalarBinary(await valueWithPullback(
        at: Float(row.a), Float(row.b), of: { a, b in
          await cfgFloat(a, b, flag)
        }), row)
    }
  }

  tests.test("Loops/Inclusive/PerIterationPullbacks") {
    for entry in loopExpectations {
      let flag = entry.flag
      let n = entry.n
      for row in [entry.first, entry.second] {
        checkScalarBinary(await valueWithPullback(
          at: Float(row.a), Float(row.b), of: { a, b in
            await loopFloat(a, b, flag, n)
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
          at: Float(row.a), Float(row.b), of: { a, b in
            await ternaryFloat(a, b, flag, n)
          }), row)
      }
    }
  }

  tests.test("ThreeDistinctCaptures/ZeroThirdGradient") {
    let (value, pullback) = await valueWithPullback(
      at: Float(3), Float(5), Float(19), of: threeCapturesFloat)
    expectEqual(Float(15), value)
    for seed: Float in [1, 2, 0, -1, 1] {
      let (da, db, dc) = pullback(seed)
      expectEqual(5 * seed, da)
      expectEqual(3 * seed, db)
      expectEqual(Float(0), dc)
    }
  }

  tests.test("TaskIdentity") {
    checkScalarUnary(await valueWithPullback(at: Float(3), of: identityChildFloat),
                     value: 3, derivative: 1)
  }

  #if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)
  tests.test("InactiveBlockingWork") {
    for row in sleepExpectations {
      checkScalarBinary(await valueWithPullback(
        at: Float(row.a), Float(row.b), of: sleepWorkFloat), row)
    }
  }
  #endif
}

func registerDoubleTests() {
  let tests = TestSuite("AsyncVJP/" + "Double")

  tests.test("PlainIdentity") {
    for x: Double in [3, -2, 0] {
      checkScalarUnary(await valueWithPullback(at: x, of: plainDouble),
                       value: x, derivative: 1)
    }
  }

  tests.test("TaskSquare") {
    for x: Double in [3, -2, 0] {
      checkScalarUnary(await valueWithPullback(at: x, of: squareDouble),
                       value: x * x, derivative: 2 * x)
    }
  }

  tests.test("NotUseful") {
    checkScalarUnary(await valueWithPullback(at: Double(3), of: notUsefulDouble),
                     value: 0, derivative: 0)
  }

  tests.test("NotVaried") {
    let z = Double(7)
    checkScalarUnary(await valueWithPullback(at: Double(3), of: { x in
      await notVariedDouble(x, z)
    }), value: z, derivative: 0)
  }

  tests.test("ThreeChildren/ReorderedCaptures") {
    for row in threeChildExpectations {
      checkScalarBinary(await valueWithPullback(
        at: Double(row.a), Double(row.b), of: doWorkDouble), row)
    }
  }

  tests.test("SynchronousControl") {
    for row in threeChildExpectations {
      checkScalarBinary(valueWithPullback(
        at: Double(row.a), Double(row.b), of: doWorkSyncDouble), row)
    }
  }

  tests.test("Branches/BothArms") {
    for entry in branchExpectations {
      let flag = entry.flag
      let row = entry.result
      checkScalarBinary(await valueWithPullback(
        at: Double(row.a), Double(row.b), of: { a, b in
          await cfgDouble(a, b, flag)
        }), row)
    }
  }

  tests.test("Loops/Inclusive/PerIterationPullbacks") {
    for entry in loopExpectations {
      let flag = entry.flag
      let n = entry.n
      for row in [entry.first, entry.second] {
        checkScalarBinary(await valueWithPullback(
          at: Double(row.a), Double(row.b), of: { a, b in
            await loopDouble(a, b, flag, n)
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
          at: Double(row.a), Double(row.b), of: { a, b in
            await ternaryDouble(a, b, flag, n)
          }), row)
      }
    }
  }

  tests.test("ThreeDistinctCaptures/ZeroThirdGradient") {
    let (value, pullback) = await valueWithPullback(
      at: Double(3), Double(5), Double(19), of: threeCapturesDouble)
    expectEqual(Double(15), value)
    for seed: Double in [1, 2, 0, -1, 1] {
      let (da, db, dc) = pullback(seed)
      expectEqual(5 * seed, da)
      expectEqual(3 * seed, db)
      expectEqual(Double(0), dc)
    }
  }

  tests.test("TaskIdentity") {
    checkScalarUnary(await valueWithPullback(at: Double(3), of: identityChildDouble),
                     value: 3, derivative: 1)
  }

  #if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)
  tests.test("InactiveBlockingWork") {
    for row in sleepExpectations {
      checkScalarBinary(await valueWithPullback(
        at: Double(row.a), Double(row.b), of: sleepWorkDouble), row)
    }
  }
  #endif
}

@main
struct Main {
  static func main() async {
    registerFloatTests()
    registerDoubleTests()
    await runAllTestsAsync()
  }
}
