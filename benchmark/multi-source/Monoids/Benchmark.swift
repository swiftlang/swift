import TestsUtils
import Dispatch

public let benchmarks = [
  BenchmarkInfo(
    name: "Monoids",
    runFunction: run_Monoids,
    tags: [.algorithm])
]

func run_Monoids(_ n: Int) {
  let semaphore = DispatchSemaphore(value: 0)
  for _ in 0 ... n {
    Task {
      await run(output: false)
      semaphore.signal()
    }
    semaphore.wait()
  }
}
