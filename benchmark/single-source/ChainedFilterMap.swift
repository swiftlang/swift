import TestsUtils

public var ChainedFilterMap = [
  BenchmarkInfo(name: "ChainedFilterMap", runFunction: run_ChainedFilterMap,
    tags: [.algorithm], setUpFunction: { blackHole(first100k) },
    legacyFactor: 9),
  BenchmarkInfo(name: "FatCompactMap", runFunction: run_FatCompactMap,
    tags: [.algorithm], setUpFunction: { blackHole(first100k) },
    legacyFactor: 10)
]

public let first100k = Array(0...100_000-1)

@inline(never)
public func run_ChainedFilterMap(_ N: Int) {
  var result = 0
  for _ in 1...N {
    let numbers = first100k.lazy
      .filter { $0 % 3 == 0 }
      .map { $0 * 2 }
      .filter { $0 % 8 == 0 }
      .map { $0 / 2 + 1}
    result = numbers.reduce(into: 0) { $0 += $1 }
  }

  CheckResults(result == 416691666)
}

@inline(never)
public func run_FatCompactMap(_ N: Int) {
  var result = 0
  for _ in 1...N {
    let numbers = first100k.lazy
      .compactMap { (x: Int) -> Int? in
        if x % 3 != 0 { return nil }
        let y = x * 2
        if y % 8 != 0 { return nil }
        let z = y / 2 + 1
        return z
      }
    result = numbers.reduce(into: 0) { $0 += $1 }
  }
  CheckResults(result == 416691666)
}
