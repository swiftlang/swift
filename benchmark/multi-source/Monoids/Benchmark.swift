import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "Monoids",
    runFunction: run_Monoids,
    tags: [.algorithm, .miniapplication, .long])
]

func run_Monoids(_ n: Int) async {
  for _ in 0 ... n {
    await run(output: false)
  }
}
