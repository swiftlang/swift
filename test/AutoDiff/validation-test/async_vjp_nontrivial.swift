// RUN: %target-run-simple-swift(-Onone -swift-version 5 -parse-as-library -whole-module-optimization -Xfrontend -disable-availability-checking -Xfrontend -sil-verify-all %S/Inputs/async_vjp_values.swift %S/Inputs/async_vjp_expected.swift %S/Inputs/async_vjp_nontrivial_cases.swift)
// RUN: %target-run-simple-swift(-O -swift-version 5 -parse-as-library -whole-module-optimization -Xfrontend -disable-availability-checking -Xfrontend -sil-verify-all %S/Inputs/async_vjp_values.swift %S/Inputs/async_vjp_expected.swift %S/Inputs/async_vjp_nontrivial_cases.swift)
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: differentiable_programming

import _Differentiation
import StdlibUnittest

func registerHeapTests() {
  let tests = TestSuite("AsyncVJP/Nontrivial")

  tests.test("PlainIdentity") {
    expectEqual(false, _isPOD(HeapPair.self))
    expectEqual(false, _isPOD(HeapPair.TangentVector.self))
    checkHeapUnary(await valueWithPullback(at: HeapPair(3, -2), of: plainHeap),
                   value: [3, -2], derivative: [1, 1])
  }
  tests.test("TaskSquare") {
    checkHeapUnary(await valueWithPullback(at: HeapPair(3, -2), of: squareHeap),
                   value: [9, 4], derivative: [6, -4])
  }
  tests.test("NotUseful") {
    checkHeapUnary(await valueWithPullback(at: HeapPair(3, -2), of: notUsefulHeap),
                   value: [0, 0], derivative: [0, 0])
  }
  tests.test("NotVaried") {
    let z = HeapPair(7, 11)
    checkHeapUnary(await valueWithPullback(at: HeapPair(3, -2), of: { x in
      await notVariedHeap(x, z)
    }), value: [7, 11], derivative: [0, 0])
  }
  tests.test("ThreeChildren/ReorderedCaptures") {
    // Each lane executes the corresponding scalar polynomial independently.
    checkHeapBinary(await valueWithPullback(
      at: HeapPair(3, 2), HeapPair(5, -1), of: doWorkHeap),
      first: threeChildExpectations[0], second: threeChildExpectations[2])
    checkHeapBinary(await valueWithPullback(
      at: HeapPair(5, 0), HeapPair(3, 5), of: doWorkHeap),
      first: threeChildExpectations[1], second: threeChildExpectations[3])
  }
  tests.test("SynchronousControl") {
    checkHeapBinary(valueWithPullback(
      at: HeapPair(3, 2), HeapPair(5, -1), of: doWorkSyncHeap),
      first: threeChildExpectations[0], second: threeChildExpectations[2])
  }
  tests.test("Branches/BothArms") {
    for offset in [0, 4] {
      let flag = branchExpectations[offset].flag
      checkHeapBinary(await valueWithPullback(
        at: HeapPair(3, 2), HeapPair(5, -1), of: { a, b in
          await cfgHeap(a, b, flag)
        }), first: branchExpectations[offset].result,
        second: branchExpectations[offset + 2].result)
    }
  }
  tests.test("Loops/Inclusive/PerIterationPullbacks") {
    for entry in loopExpectations {
      let flag = entry.flag
      let n = entry.n
      checkHeapBinary(await valueWithPullback(
        at: HeapPair(3, 2), HeapPair(5, -1), of: { a, b in
          await loopHeap(a, b, flag, n)
        }), first: entry.first, second: entry.second)
    }
  }
  tests.test("Loops/UnusedThirdWorkloadArgument") {
    for entry in loopExpectations {
      let flag = entry.flag
      let n = entry.n
      checkHeapBinary(await valueWithPullback(
        at: HeapPair(3, 2), HeapPair(5, -1), of: { a, b in
          await ternaryHeap(a, b, flag, n)
        }), first: entry.first, second: entry.second)
    }
  }
  tests.test("ThreeDistinctCaptures/ZeroThirdGradient") {
    let (value, pullback) = await valueWithPullback(
      at: HeapPair(3, 2), HeapPair(5, -1), HeapPair(19, 23),
      of: threeCapturesHeap)
    expectEqual([15, -2], value.values)
    for seed in heapSeeds {
      let (da, db, dc) = pullback(heapSeed(seed))
      expectHeapTangent([5 * seed[0], -seed[1]], da)
      expectHeapTangent([3 * seed[0], 2 * seed[1]], db)
      expectHeapTangent([0, 0], dc)
    }
  }
  tests.test("TaskIdentity/NontrivialSeedAndResult") {
    checkHeapUnary(await valueWithPullback(
      at: HeapPair(3, -2), of: identityChildHeap),
      value: [3, -2], derivative: [1, 1])
  }
  tests.test("SavedPullbackSurvivesInputMutation") {
    var input = HeapPair(3, -2)
    let result = await valueWithPullback(at: input, of: squareHeap)
    input.values[0] = 100
    input.values[1] = 200
    expectEqual([100, 200], input.values)
    // All child-result storage has been cleaned up before these calls.
    checkHeapUnary(result, value: [9, 4], derivative: [6, -4])
  }
  #if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)
  tests.test("InactiveBlockingWork") {
    checkHeapBinary(await valueWithPullback(
      at: HeapPair(3, 2), HeapPair(5, -1), of: sleepWorkHeap),
      first: sleepExpectations[0], second: sleepExpectations[2])
  }
  #endif
}

@main
struct Main {
  static func main() async {
    registerHeapTests()
    await runAllTestsAsync()
  }
}
