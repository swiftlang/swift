//===--- DriverUtils.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if os(Linux)
import Glibc
#else
import Darwin
import LibProc
#endif

import TestsUtils

struct BenchResults {
  typealias T = Int
  private let samples: [T]
  let maxRSS: Int
  let stats: Stats

  init(_ samples: [T], maxRSS: Int) {
    self.samples = samples.sorted()
    self.maxRSS = maxRSS
    self.stats = self.samples.reduce(into: Stats(), Stats.collect)
  }

  /// Return sample at index nearest to the `quantile`.
  subscript(_ quantile: Double) -> T {
    let index = Int((Double(samples.count - 1) * quantile).rounded())
    return samples[index]
  }

  var sampleCount: T { return samples.count }
  var min: T { return samples.first! }
  var max: T { return samples.last! }
  var mean: T { return Int(stats.mean.rounded()) }
  var sd: T { return Int(stats.standardDeviation.rounded()) }
  var median: T { return self[0.5] }
}

public var registeredBenchmarks: [BenchmarkInfo] = []

enum TestAction {
  case run
  case listTests
}

struct TestConfig {
  /// The delimiter to use when printing output.
  let delim: String

  /// Duration of the test measurement in seconds.
  ///
  /// Used to compute the number of iterations, if no fixed amount is specified.
  /// This is useful when one wishes for a test to run for a
  /// longer amount of time to perform performance analysis on the test in
  /// instruments.
  let sampleTime: Double

  /// If we are asked to have a fixed number of iterations, the number of fixed
  /// iterations. The default value of 0 means: automatically compute the
  /// number of iterations to measure the test for a specified sample time.
  let fixedNumIters: Int

  /// The number of samples we should take of each test.
  let numSamples: Int

  /// Is verbose output enabled?
  let verbose: Bool

  // Should we log the test's memory usage?
  let logMemory: Bool

  /// After we run the tests, should the harness sleep to allow for utilities
  /// like leaks that require a PID to run on the test harness.
  let afterRunSleep: UInt32?

  /// The list of tests to run.
  let tests: [(index: String, info: BenchmarkInfo)]

  let action: TestAction

  init(_ registeredBenchmarks: [BenchmarkInfo]) {

    struct PartialTestConfig {
      var delim: String?
      var tags, skipTags: Set<BenchmarkCategory>?
      var numSamples: UInt?
      var fixedNumIters: UInt?
      var afterRunSleep: UInt32?
      var sampleTime: Double?
      var verbose: Bool?
      var logMemory: Bool?
      var action: TestAction?
      var tests: [String]?
    }

    // Custom value type parsers
    func tags(tags: String) throws -> Set<BenchmarkCategory> {
      // We support specifying multiple tags by splitting on comma, i.e.:
      //  --tags=Array,Dictionary
      //  --skip-tags=Array,Set,unstable,skip
      return Set(
        try tags.split(separator: ",").map(String.init).map {
          try checked({ BenchmarkCategory(rawValue: $0) }, $0) })
    }
    func finiteDouble(value: String) -> Double? {
      return Double(value).flatMap { $0.isFinite ? $0 : nil }
    }

    // Configure the command line argument parser
    let p = ArgumentParser(into: PartialTestConfig())
    p.addArgument("--num-samples", \.numSamples,
                  help: "number of samples to take per benchmark; default: 1",
                  parser: { UInt($0) })
    p.addArgument("--num-iters", \.fixedNumIters,
                  help: "number of iterations averaged in the sample;\n" +
                        "default: auto-scaled to measure for `sample-time`",
                  parser: { UInt($0) })
    p.addArgument("--sample-time", \.sampleTime,
                  help: "duration of test measurement in seconds\ndefault: 1",
                  parser: finiteDouble)
    p.addArgument("--verbose", \.verbose, defaultValue: true,
                  help: "increase output verbosity")
    p.addArgument("--memory", \.logMemory, defaultValue: true,
                  help: "log the change in maximum resident set size (MAX_RSS)")
    p.addArgument("--delim", \.delim,
                  help:"value delimiter used for log output; default: ,",
                  parser: { $0 })
    p.addArgument("--tags", \PartialTestConfig.tags,
                  help: "run tests matching all the specified categories",
                  parser: tags)
    p.addArgument("--skip-tags", \PartialTestConfig.skipTags, defaultValue: [],
                  help: "don't run tests matching any of the specified\n" +
                        "categories; default: unstable,skip",
                  parser: tags)
    p.addArgument("--sleep", \.afterRunSleep,
                  help: "number of seconds to sleep after benchmarking",
                  parser: { UInt32($0) })
    p.addArgument("--list", \.action, defaultValue: .listTests,
                  help: "don't run the tests, just log the list of test \n" +
                        "numbers, names and tags (respects specified filters)")
    p.addArgument(nil, \.tests) // positional arguments

    let c = p.parse()

    // Configure from the command line arguments, filling in the defaults.
    delim = c.delim ?? ","
    sampleTime = c.sampleTime ?? 1.0
    fixedNumIters = Int(c.fixedNumIters ?? 0)
    numSamples = Int(c.numSamples ?? 1)
    verbose = c.verbose ?? false
    logMemory = c.logMemory ?? false
    afterRunSleep = c.afterRunSleep
    action = c.action ?? .run
    tests = TestConfig.filterTests(registeredBenchmarks,
                                    specifiedTests: Set(c.tests ?? []),
                                    tags: c.tags ?? [],
                                    skipTags: c.skipTags ?? [.unstable, .skip])

    if logMemory && tests.count > 1 {
      print(
      """
      warning: The memory usage of a test, reported as the change in MAX_RSS,
               is based on measuring the peak memory used by the whole process.
               These results are meaningful only when running a single test,
               not in the batch mode!
      """)
    }

    if verbose {
      let testList = tests.map({ $0.1.name }).joined(separator: ", ")
      print("""
            --- CONFIG ---
            NumSamples: \(numSamples)
            Verbose: \(verbose)
            LogMemory: \(logMemory)
            SampleTime: \(sampleTime)
            FixedIters: \(fixedNumIters)
            Delimiter: \(String(reflecting: delim))
            Tests Filter: \(c.tests ?? [])
            Tests to run: \(testList)

            --- DATA ---\n
            """)
    }
  }

  /// Returns the list of tests to run.
  ///
  /// - Parameters:
  ///   - registeredBenchmarks: List of all performance tests to be filtered.
  ///   - specifiedTests: List of explicitly specified tests to run. These can
  ///     be specified either by a test name or a test number.
  ///   - tags: Run tests tagged with all of these categories.
  ///   - skipTags: Don't run tests tagged with any of these categories.
  /// - Returns: An array of test number and benchmark info tuples satisfying
  ///     specified filtering conditions.
  static func filterTests(
    _ registeredBenchmarks: [BenchmarkInfo],
    specifiedTests: Set<String>,
    tags: Set<BenchmarkCategory>,
    skipTags: Set<BenchmarkCategory>
  ) -> [(index: String, info: BenchmarkInfo)] {
    let allTests = registeredBenchmarks.sorted()
    let indices = Dictionary(uniqueKeysWithValues:
      zip(allTests.map { $0.name },
          (1...).lazy.map { String($0) } ))

    func byTags(b: BenchmarkInfo) -> Bool {
      return b.tags.isSuperset(of: tags) &&
        b.tags.isDisjoint(with: skipTags)
    }
    func byNamesOrIndices(b: BenchmarkInfo) -> Bool {
      return specifiedTests.contains(b.name) ||
        specifiedTests.contains(indices[b.name]!)
    } // !! "`allTests` have been assigned an index"
    return allTests
      .filter(specifiedTests.isEmpty ? byTags : byNamesOrIndices)
      .map { (index: indices[$0.name]!, info: $0) }
  }
}

struct Stats {
    var n: Int = 0
    var S: Double = 0.0
    var mean: Double = 0.0
    var variance: Double { return n < 2 ? 0.0 : S / Double(n - 1) }
    var standardDeviation: Double { return variance.squareRoot() }

    static func collect(_ s: inout Stats, _ x: Int){
        Stats.runningMeanVariance(&s, Double(x))
    }

    /// Compute running mean and variance using B. P. Welford's method.
    ///
    /// See Knuth TAOCP vol 2, 3rd edition, page 232, or
    /// https://www.johndcook.com/blog/standard_deviation/
    static func runningMeanVariance(_ s: inout Stats, _ x: Double){
        let n = s.n + 1
        let (k, M_, S_) = (Double(n), s.mean, s.S)
        let M = M_ + (x - M_) / k
        let S = S_ + (x - M_) * (x - M)
        (s.n, s.mean, s.S) = (n, M, S)
    }
}

#if SWIFT_RUNTIME_ENABLE_LEAK_CHECKER

@_silgen_name("_swift_leaks_startTrackingObjects")
func startTrackingObjects(_: UnsafePointer<CChar>) -> ()
@_silgen_name("_swift_leaks_stopTrackingObjects")
func stopTrackingObjects(_: UnsafePointer<CChar>) -> Int

#endif

final class Timer {
#if os(Linux)
  typealias TimeT = timespec

  func getTime() -> TimeT {
    var ts = timespec(tv_sec: 0, tv_nsec: 0)
    clock_gettime(CLOCK_REALTIME, &ts)
    return ts
  }

  func diffTimeInNanoSeconds(from start: TimeT, to end: TimeT) -> UInt64 {
    let oneSecond = 1_000_000_000 // ns
    var elapsed = timespec(tv_sec: 0, tv_nsec: 0)
    if end.tv_nsec - start.tv_nsec < 0 {
      elapsed.tv_sec = end.tv_sec - start.tv_sec - 1
      elapsed.tv_nsec = end.tv_nsec - start.tv_nsec + oneSecond
    } else {
      elapsed.tv_sec = end.tv_sec - start.tv_sec
      elapsed.tv_nsec = end.tv_nsec - start.tv_nsec
    }
    return UInt64(elapsed.tv_sec) * UInt64(oneSecond) + UInt64(elapsed.tv_nsec)
  }
#else
  typealias TimeT = UInt64
  var info = mach_timebase_info_data_t(numer: 0, denom: 0)

  init() {
    mach_timebase_info(&info)
  }

  func getTime() -> TimeT {
    return mach_absolute_time()
  }

  func diffTimeInNanoSeconds(from start: TimeT, to end: TimeT) -> UInt64 {
    let elapsed = end - start
    return elapsed * UInt64(info.numer) / UInt64(info.denom)
  }
#endif
}

final class SampleRunner {
  let timer = Timer()
  let baseline = SampleRunner.getResourceUtilization()
  let c: TestConfig
  var start, end, lastYield: Timer.TimeT
  let schedulerQuantum = UInt64(10_000_000) // nanoseconds (== 10ms, macos)

  init(_ config: TestConfig) {
    self.c = config
    sched_yield()
    let now = timer.getTime()
    (start, end, lastYield) = (now, now, now)
  }

#if os(Linux)
  private static func getExecutedInstructions() -> UInt64 {
    // FIXME: there is a Linux PMC API you can use to get this, but it's
    // not quite so straightforward.
    return 0
  }
#else
  private static func getExecutedInstructions() -> UInt64 {
    if #available(OSX 10.9, iOS 7.0, *) {
      var u = rusage_info_v4()
      let p = UnsafeMutablePointer(&u)
      p.withMemoryRebound(to: Optional<rusage_info_t>.self, capacity: 1) { up in
        let _ = proc_pid_rusage(getpid(), RUSAGE_INFO_V4, up)
      }
      return u.ri_instructions
    } else {
      return 0
    }
  }
#endif

  private static func getResourceUtilization() -> rusage {
    var u = rusage(); getrusage(RUSAGE_SELF, &u); return u
  }

  /// Returns maximum resident set size (MAX_RSS) delta in bytes
  func measureMemoryUsage() -> Int {
    let current = SampleRunner.getResourceUtilization()
    let maxRSS = current.ru_maxrss - baseline.ru_maxrss

    if c.verbose {
      let pages = maxRSS / sysconf(_SC_PAGESIZE)
      func deltaEquation(_ stat: KeyPath<rusage, Int>) -> String {
        let b = baseline[keyPath: stat], c = current[keyPath: stat]
        return "\(c) - \(b) = \(c - b)"
      }
      print("""
                MAX_RSS \(deltaEquation(\rusage.ru_maxrss)) (\(pages) pages)
                ICS \(deltaEquation(\rusage.ru_nivcsw))
                VCS \(deltaEquation(\rusage.ru_nvcsw))
            """)
    }
    return maxRSS
  }

  private func startMeasurement() {
    let spent = timer.diffTimeInNanoSeconds(from: lastYield, to: end)
    let nextSampleEstimate = UInt64(Double(lastSampleTime) * 1.5)

    if (spent + nextSampleEstimate < schedulerQuantum) {
        start = timer.getTime()
    } else {
        if c.verbose {
          print("    Yielding again after estimated \(spent/1000) us")
        }
        sched_yield()
        let now = timer.getTime()
        (start, lastYield) = (now, now)
    }
  }

  private func stopMeasurement() {
    end = timer.getTime()
  }

  /// Time in nanoseconds spent running the last function
  var lastSampleTime: UInt64 {
    return timer.diffTimeInNanoSeconds(from: start, to: end)
  }

  /// Measure the `fn` and return the average sample time per iteration (μs).
  func measure(_ name: String, fn: (Int) -> Void, numIters: Int) -> Int {
#if SWIFT_RUNTIME_ENABLE_LEAK_CHECKER
    name.withCString { p in startTrackingObjects(p) }
#endif

    startMeasurement()
    fn(numIters)
    stopMeasurement()

#if SWIFT_RUNTIME_ENABLE_LEAK_CHECKER
    name.withCString { p in stopTrackingObjects(p) }
#endif

    // Convert to μs and compute the average sample time per iteration.
    return Int(lastSampleTime / 1000) / numIters
  }
}

/// Invoke the benchmark entry point and return the run time in milliseconds.
func runBench(_ test: BenchmarkInfo, _ c: TestConfig) -> BenchResults? {
  func logVerbose(_ msg: @autoclosure () -> String) {
    if c.verbose { print(msg()) }
  }
  var samples = [Int](repeating: 0, count: c.numSamples)

  // Before we do anything, check that we actually have a function to
  // run. If we don't it is because the benchmark is not supported on
  // the platform and we should skip it.
  guard let testFn = test.runFunction else {
    logVerbose("Skipping unsupported benchmark \(test.name)!")
    return nil
  }

  logVerbose("Running \(test.name) for \(c.numSamples) samples.")

  let sampler = SampleRunner(c)
  test.setUpFunction?()

  for s in 0..<c.numSamples {
    var numIters : Int
    var time: Int = 0
    if c.fixedNumIters == 0 {
      // Compute the `numIters` if a `c.fixedNumIters` is not specified.
      time = sampler.measure(test.name, fn: testFn, numIters: 1)

      if time > 0 {
        let usPerSecond = 1_000_000.0 // microseconds (μs)
        let timePerSample = Int(c.sampleTime * usPerSecond)
        // Number of iterations to make `testFn` run for the desired time.
        numIters = timePerSample / time
      } else {
        logVerbose("    Warning: elapsed time is 0!")
        numIters = 1
      }
    } else {
      numIters = c.fixedNumIters
      if numIters == 1 {
        time = sampler.measure(test.name, fn: testFn, numIters: 1)
      }
    }
    // Cap the `numIters` to prevent overflow on 32-bit systems during
    // multiplication with the inner loop multiplier inside the `testFn`.
    numIters = min(numIters, Int.max / 10_000)

    // Rerun the test with the computed scale factor.
    if numIters > 1 {
      logVerbose("    Measuring with scale \(numIters).")
      time = sampler.measure(test.name, fn: testFn, numIters: numIters)
    }

    samples[s] = time
    logVerbose("    Sample \(s),\(samples[s])")
  }
  test.tearDownFunction?()

  return BenchResults(samples, maxRSS: sampler.measureMemoryUsage())
}

/// Execute benchmarks and continuously report the measurement results.
func runBenchmarks(_ c: TestConfig) {
  let withUnit = {$0 + "(us)"}
  let header = (
    ["#", "TEST", "SAMPLES"] +
    ["MIN", "MAX", "MEAN", "SD", "MEDIAN"].map(withUnit)
    + (c.logMemory ? ["MAX_RSS(B)"] : [])
  ).joined(separator: c.delim)
  print(header)

  var testCount = 0

  func report(_ index: String, _ t: BenchmarkInfo, results: BenchResults?) {
    func values(r: BenchResults) -> [String] {
      return ([r.sampleCount, r.min, r.max, r.mean, r.sd, r.median] +
              (c.logMemory ? [r.maxRSS] : [])).map { String($0) }
    }
    let benchmarkStats = (
      [index, t.name] + (results.map(values) ?? ["Unsupported"])
    ).joined(separator: c.delim)

    print(benchmarkStats)
    fflush(stdout)

    if (results != nil) {
      testCount += 1
    }
  }

  for (index, test) in c.tests {
    report(index, test, results:runBench(test, c))
  }

  print("\nTotal performance tests executed: \(testCount)")
}

public func main() {
  let config = TestConfig(registeredBenchmarks)
  switch (config.action) {
  case .listTests:
    print("#\(config.delim)Test\(config.delim)[Tags]")
    for (index, t) in config.tests {
      let testDescription = [index, t.name, t.tags.sorted().description]
        .joined(separator: config.delim)
      print(testDescription)
    }
  case .run:
    runBenchmarks(config)
    if let x = config.afterRunSleep {
      sleep(x)
    }
  }
}
