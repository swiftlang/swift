import _Differentiation
import StdlibUnittest

// Small integers keep every expected operation exactly representable in Float
// and Double. These constants are independent of both async and synchronous AD.
struct BinaryExpectation {
  let a: Double
  let b: Double
  let value: Double
  let da: Double
  let db: Double
  init(_ a: Double, _ b: Double, _ value: Double, _ da: Double, _ db: Double) {
    self.a = a
    self.b = b
    self.value = value
    self.da = da
    self.db = db
  }
}
struct BranchExpectation {
  let flag: Bool
  let result: BinaryExpectation
}
struct LoopExpectation {
  let flag: Bool
  let n: Int
  let first: BinaryExpectation
  let second: BinaryExpectation
}

let threeChildExpectations: [BinaryExpectation] = [
  .init(3, 5, 233, 151, 91),
  .init(5, 3, 233, 91, 151),
  .init(2, -1, 5, 5, -7),
  .init(0, 5, 5, 1, 1),
]

let sleepExpectations: [BinaryExpectation] = [
  .init(3, 5, 23, 6, 4),
  .init(5, 3, 23, 4, 6),
  .init(2, -1, -1, 0, 3),
  .init(0, 5, 5, 6, 1),
]

let branchExpectations: [BranchExpectation] = [
  .init(flag: true, result: .init(3, 5, 233, 151, 91)),
  .init(flag: true, result: .init(5, 3, 233, 91, 151)),
  .init(flag: true, result: .init(2, -1, 5, 5, -7)),
  .init(flag: true, result: .init(0, 5, 5, 1, 1)),
  .init(flag: false, result: .init(3, 5, 128, 56, 40)),
  .init(flag: false, result: .init(5, 3, 128, 40, 56)),
  .init(flag: false, result: .init(2, -1, -1, -2, 1)),
  .init(flag: false, result: .init(0, 5, 5, 26, 1)),
]

// n == 0 means ONE iteration, not an empty loop.
let loopExpectations: [LoopExpectation] = [
  .init(flag: true, n: 0,
        first: .init(3, 5, 265, 155, 103), second: .init(2, -1, 3, 3, -8)),
  .init(flag: false, n: 0,
        first: .init(3, 5, 133, 56, 41), second: .init(2, -1, -2, -2, 2)),
  .init(flag: true, n: 1,
        first: .init(3, 5, 329, 171, 119), second: .init(2, -1, 4, 5, -6)),
  .init(flag: false, n: 1,
        first: .init(3, 5, 141, 57, 42), second: .init(2, -1, -1, -1, 3)),
  .init(flag: true, n: 3,
        first: .init(3, 5, 529, 236, 160), second: .init(2, -1, 12, 18, 4)),
  .init(flag: false, n: 3,
        first: .init(3, 5, 166, 62, 44), second: .init(2, -1, 7, 4, 5)),
]

// Every call below is synchronous. Reusing one closure after a zero or negative
// seed catches consumed/mutated saved state that a single gradient(1) misses.
func checkScalarUnary<S: BinaryFloatingPoint>(
  _ result: (value: S, pullback: (S) -> S),
  value: S, derivative: S,
  file: String = #file, line: UInt = #line
) {
  expectEqual(value, result.value, file: file, line: line)
  let pullback: (S) -> S = result.pullback
  for seed: S in [1, 2, 0, -1, 1] {
    expectEqual(seed * derivative, pullback(seed), "seed=\(seed)",
                file: file, line: line)
  }
}

func checkScalarBinary<S: BinaryFloatingPoint>(
  _ result: (value: S, pullback: (S) -> (S, S)),
  _ expected: BinaryExpectation,
  file: String = #file, line: UInt = #line
) {
  expectEqual(S(expected.value), result.value, file: file, line: line)
  let pullback: (S) -> (S, S) = result.pullback
  for seed: S in [1, 2, 0, -1, 1] {
    let (da, db) = pullback(seed)
    let message = "a=\(expected.a), b=\(expected.b), seed=\(seed)"
    expectEqual(seed * S(expected.da), da, message, file: file, line: line)
    expectEqual(seed * S(expected.db), db, message, file: file, line: line)
  }
}

// Basis seeds check lane independence; the mixed seed checks sign and scaling.
let heapSeeds: [[Double]] = [[1, 0], [0, 1], [2, -3], [0, 0], [1, 0]]

func heapSeed(_ values: [Double]) -> HeapPair.TangentVector {
  HeapPair.TangentVector(values: .init(values))
}

func expectHeapTangent(
  _ expected: [Double], _ actual: HeapPair.TangentVector,
  _ message: String = "", file: String = #file, line: UInt = #line
) {
  let base = actual.values.base
  // Array's tangent .zero is represented by an empty DifferentiableView.
  // Accept that representation only as zero in BOTH lanes, never as a wildcard.
  expectEqual(expected, base.isEmpty ? [0, 0] : base, message,
              file: file, line: line)
}

func checkHeapUnary(
  _ result: (value: HeapPair,
             pullback: (HeapPair.TangentVector) -> HeapPair.TangentVector),
  value: [Double], derivative: [Double],
  file: String = #file, line: UInt = #line
) {
  expectEqual(value, result.value.values, file: file, line: line)
  for seed in heapSeeds {
    expectHeapTangent([seed[0] * derivative[0], seed[1] * derivative[1]],
                      result.pullback(heapSeed(seed)), "seed=\(seed)",
                      file: file, line: line)
  }
}

func checkHeapBinary(
  _ result: (value: HeapPair,
             pullback: (HeapPair.TangentVector) ->
               (HeapPair.TangentVector, HeapPair.TangentVector)),
  first: BinaryExpectation, second: BinaryExpectation,
  file: String = #file, line: UInt = #line
) {
  expectEqual([first.value, second.value], result.value.values,
              file: file, line: line)
  for seed in heapSeeds {
    let (da, db) = result.pullback(heapSeed(seed))
    expectHeapTangent([seed[0] * first.da, seed[1] * second.da], da,
                      "da, seed=\(seed)", file: file, line: line)
    expectHeapTangent([seed[0] * first.db, seed[1] * second.db], db,
                      "db, seed=\(seed)", file: file, line: line)
  }
}
