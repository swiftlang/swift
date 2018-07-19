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
#endif

import TestsUtils

struct BenchResults {
  let sampleCount, min, max, mean, sd, median, maxRSS: UInt64
}

public var registeredBenchmarks: [BenchmarkInfo] = []

enum TestAction {
  case run
  case listTests
  case help([String])
}

struct TestConfig {
  /// The delimiter to use when printing output.
  var delim: String  = ","

  /// The filters applied to our test names.
  var filters = [String]()

  /// The tags that we want to run
  var tags = Set<BenchmarkCategory>()

  /// Tests tagged with any of these will not be executed
  var skipTags: Set<BenchmarkCategory> = [.unstable, .skip]

  /// The scalar multiple of the amount of times a test should be run. This
  /// enables one to cause tests to run for N iterations longer than they
  /// normally would. This is useful when one wishes for a test to run for a
  /// longer amount of time to perform performance analysis on the test in
  /// instruments.
  var iterationScale: Int = 1

  /// If we are asked to have a fixed number of iterations, the number of fixed
  /// iterations.
  var fixedNumIters: UInt = 0

  /// The number of samples we should take of each test.
  var numSamples: Int = 1

  /// Is verbose output enabled?
  var verbose: Bool = false

  /// After we run the tests, should the harness sleep to allow for utilities
  /// like leaks that require a PID to run on the test harness.
  var afterRunSleep: Int?

  /// The list of tests to run.
  var tests = [(index: String, info: BenchmarkInfo)]()

  mutating func processArguments() throws -> TestAction {
    let validOptions = [
      "--iter-scale", "--num-samples", "--num-iters",
      "--verbose", "--delim", "--list", "--sleep",
      "--tags", "--skip-tags", "--help"
    ]
    guard let benchArgs = parseArgs(validOptions) else {
      throw ArgumentError.general("Failed to parse arguments")
    }

    filters = benchArgs.positionalArgs

    if benchArgs.optionalArgsMap["--help"] != nil {
      return .help(validOptions)
    }

    func optionalArg(_ name: String, _ action: (String) throws -> Void) throws {
      if let value = benchArgs.optionalArgsMap[name] {
        guard !value.isEmpty else { throw ArgumentError.missingValue(name) }
        try action(value)
      }
    }
    try optionalArg("--iter-scale") { iterationScale = Int($0)! }
    try optionalArg("--num-iters") { fixedNumIters = UInt($0)! }
    try optionalArg("--num-samples") { numSamples = Int($0)! }

    if let _ = benchArgs.optionalArgsMap["--verbose"] {
      verbose = true
      print("Verbose")
    }

    try optionalArg("--delim") { delim = $0 }

    func parseCategory(tag: String) throws -> BenchmarkCategory {
      guard let category = BenchmarkCategory(rawValue: tag) else {
        throw ArgumentError.general("Unknown benchmark category: '\(tag)'")
      }
      return category
    }

    try optionalArg("--tags") {
      // We support specifying multiple tags by splitting on comma, i.e.:
      //  --tags=Array,Dictionary
      tags = Set(
        try $0.split(separator: ",").map(String.init).map(parseCategory))
    }

    if let x = benchArgs.optionalArgsMap["--skip-tags"] {
      // if the --skip-tags parameter is specified, we need to ignore the
      // default and start from a clean slate.
      // If the parameter's value is empty, $0.split maps into []

      // We support specifying multiple tags by splitting on comma, i.e.:
      //  --skip-tags=Array,Set,unstable,skip
      skipTags = Set(
        try x.split(separator: ",").map(String.init).map(parseCategory))
    }

    if let x = benchArgs.optionalArgsMap["--sleep"] {
      guard let v = Int(x) else {
        throw ArgumentError.missingValue("--sleep")
      }
      afterRunSleep = v
    }

    if let _ = benchArgs.optionalArgsMap["--list"] {
      return .listTests
    }

    return .run
  }

  mutating func findTestsToRun() {
    registeredBenchmarks.sort()
    let indices = Dictionary(uniqueKeysWithValues:
      zip(registeredBenchmarks.map{ $0.name },
          (1...).lazy.map { String($0) } ))
    let benchmarkNamesOrIndices = Set(filters)
    // needed so we don't capture an ivar of a mutable inout self.
    let (_tags, _skipTags) = (tags, skipTags)

    tests = registeredBenchmarks.filter { benchmark in
      if benchmarkNamesOrIndices.isEmpty {
        return benchmark.tags.isSuperset(of: _tags) &&
          benchmark.tags.isDisjoint(with: _skipTags)
      } else {
        return benchmarkNamesOrIndices.contains(benchmark.name) ||
          benchmarkNamesOrIndices.contains(indices[benchmark.name]!)
      }
    }.map { (index: indices[$0.name]!, info: $0) }
  }
}

func internalMeanSD(_ inputs: [UInt64]) -> (UInt64, UInt64) {
  // If we are empty, return 0, 0.
  if inputs.isEmpty {
    return (0, 0)
  }

  // If we have one element, return elt, 0.
  if inputs.count == 1 {
    return (inputs[0], 0)
  }

  // Ok, we have 2 elements.

  var sum1: UInt64 = 0
  var sum2: UInt64 = 0

  for i in inputs {
    sum1 += i
  }

  let mean: UInt64 = sum1 / UInt64(inputs.count)

  for i in inputs {
    sum2 = sum2 &+ UInt64((Int64(i) &- Int64(mean))&*(Int64(i) &- Int64(mean)))
  }

  return (mean, UInt64(sqrt(Double(sum2)/(Double(inputs.count) - 1))))
}

func internalMedian(_ inputs: [UInt64]) -> UInt64 {
  return inputs.sorted()[inputs.count / 2]
}

#if SWIFT_RUNTIME_ENABLE_LEAK_CHECKER

@_silgen_name("_swift_leaks_startTrackingObjects")
func startTrackingObjects(_: UnsafePointer<CChar>) -> ()
@_silgen_name("_swift_leaks_stopTrackingObjects")
func stopTrackingObjects(_: UnsafePointer<CChar>) -> Int

#endif

#if os(Linux)
class Timer {
  typealias TimeT = timespec
  func getTime() -> TimeT {
    var ticks = timespec(tv_sec: 0, tv_nsec: 0)
    clock_gettime(CLOCK_REALTIME, &ticks)
    return ticks
  }
  func diffTimeInNanoSeconds(from start_ticks: TimeT, to end_ticks: TimeT) -> UInt64 {
    var elapsed_ticks = timespec(tv_sec: 0, tv_nsec: 0)
    if end_ticks.tv_nsec - start_ticks.tv_nsec < 0 {
      elapsed_ticks.tv_sec = end_ticks.tv_sec - start_ticks.tv_sec - 1
      elapsed_ticks.tv_nsec = end_ticks.tv_nsec - start_ticks.tv_nsec + 1000000000
    } else {
      elapsed_ticks.tv_sec = end_ticks.tv_sec - start_ticks.tv_sec
      elapsed_ticks.tv_nsec = end_ticks.tv_nsec - start_ticks.tv_nsec
    }
    return UInt64(elapsed_ticks.tv_sec) * UInt64(1000000000) + UInt64(elapsed_ticks.tv_nsec)
  }
}
#else
class Timer {
  typealias TimeT = UInt64
  var info = mach_timebase_info_data_t(numer: 0, denom: 0)
  init() {
    mach_timebase_info(&info)
  }
  func getTime() -> TimeT {
    return mach_absolute_time()
  }
  func diffTimeInNanoSeconds(from start_ticks: TimeT, to end_ticks: TimeT) -> UInt64 {
    let elapsed_ticks = end_ticks - start_ticks
    return elapsed_ticks * UInt64(info.numer) / UInt64(info.denom)
  }
}
#endif

class SampleRunner {
  let timer = Timer()
  let baseline = SampleRunner.usage()
  let c: TestConfig

  init(_ config: TestConfig) {
    self.c = config
  }

  private static func usage() -> rusage {
    var u = rusage(); getrusage(RUSAGE_SELF, &u); return u
  }

  /// Returns maximum resident set size (MAX_RSS) delta in bytes
  func measureMemoryUsage() -> Int {
      var current = SampleRunner.usage()
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

  func run(_ name: String, fn: (Int) -> Void, num_iters: UInt) -> UInt64 {
    // Start the timer.
#if SWIFT_RUNTIME_ENABLE_LEAK_CHECKER
    name.withCString { p in startTrackingObjects(p) }
#endif
    let start_ticks = timer.getTime()
    fn(Int(num_iters))
    // Stop the timer.
    let end_ticks = timer.getTime()
#if SWIFT_RUNTIME_ENABLE_LEAK_CHECKER
    name.withCString { p in stopTrackingObjects(p) }
#endif

    // Compute the spent time and the scaling factor.
    return timer.diffTimeInNanoSeconds(from: start_ticks, to: end_ticks)
  }
}

/// Invoke the benchmark entry point and return the run time in milliseconds.
func runBench(_ test: BenchmarkInfo, _ c: TestConfig) -> BenchResults? {
  var samples = [UInt64](repeating: 0, count: c.numSamples)

  // Before we do anything, check that we actually have a function to
  // run. If we don't it is because the benchmark is not supported on
  // the platform and we should skip it.
  guard let testFn = test.runFunction else {
    if c.verbose {
	print("Skipping unsupported benchmark \(test.name)!")
    }
    return nil
  }

  if c.verbose {
    print("Running \(test.name) for \(c.numSamples) samples.")
  }

  let sampler = SampleRunner(c)
  for s in 0..<c.numSamples {
    test.setUpFunction?()
    let time_per_sample: UInt64 = 1_000_000_000 * UInt64(c.iterationScale)

    var scale : UInt
    var elapsed_time : UInt64 = 0
    if c.fixedNumIters == 0 {
      elapsed_time = sampler.run(test.name, fn: testFn, num_iters: 1)

      if elapsed_time > 0 {
        scale = UInt(time_per_sample / elapsed_time)
      } else {
        if c.verbose {
          print("    Warning: elapsed time is 0. This can be safely ignored if the body is empty.")
        }
        scale = 1
      }
    } else {
      // Compute the scaling factor if a fixed c.fixedNumIters is not specified.
      scale = c.fixedNumIters
      if scale == 1 {
        elapsed_time = sampler.run(test.name, fn: testFn, num_iters: 1)
      }
    }
    // Make integer overflow less likely on platforms where Int is 32 bits wide.
    // FIXME: Switch BenchmarkInfo to use Int64 for the iteration scale, or fix
    // benchmarks to not let scaling get off the charts.
    scale = min(scale, UInt(Int.max) / 10_000)

    // Rerun the test with the computed scale factor.
    if scale > 1 {
      if c.verbose {
        print("    Measuring with scale \(scale).")
      }
      elapsed_time = sampler.run(test.name, fn: testFn, num_iters: scale)
    } else {
      scale = 1
    }
    // save result in microseconds or k-ticks
    samples[s] = elapsed_time / UInt64(scale) / 1000
    if c.verbose {
      print("    Sample \(s),\(samples[s])")
    }
    test.tearDownFunction?()
  }

  let (mean, sd) = internalMeanSD(samples)

  // Return our benchmark results.
  return BenchResults(sampleCount: UInt64(samples.count),
                      min: samples.min()!, max: samples.max()!,
                      mean: mean, sd: sd, median: internalMedian(samples),
                      maxRSS: UInt64(sampler.measureMemoryUsage()))
}

func printRunInfo(_ c: TestConfig) {
  if c.verbose {
    print("--- CONFIG ---")
    print("NumSamples: \(c.numSamples)")
    print("Verbose: \(c.verbose)")
    print("IterScale: \(c.iterationScale)")
    if c.fixedNumIters != 0 {
      print("FixedIters: \(c.fixedNumIters)")
    }
    print("Tests Filter: \(c.filters)")
    print("Tests to run: ", terminator: "")
    print(c.tests.map({ $0.1.name }).joined(separator: ", "))
    print("")
    print("--- DATA ---")
  }
}

/// Execute benchmarks and continuously report the measurement results.
func runBenchmarks(_ c: TestConfig) {
  let withUnit = {$0 + "(us)"}
  let header = (
    ["#", "TEST", "SAMPLES"] +
    ["MIN", "MAX", "MEAN", "SD", "MEDIAN"].map(withUnit)
    + ["MAX_RSS(B)"]
  ).joined(separator: c.delim)
  print(header)

  var testCount = 0

  func report(_ index: String, _ t: BenchmarkInfo, results: BenchResults?) {
    func values(r: BenchResults) -> [String] {
      return [r.sampleCount, r.min, r.max, r.mean, r.sd, r.median, r.maxRSS]
        .map { String($0) }
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

  print("")
  print("Totals\(c.delim)\(testCount)")
}

public func main() {
  var config = TestConfig()
  do {
    switch (try config.processArguments()) {
    case let .help(validOptions):
      print("Valid options:")
      for v in validOptions {
        print("    \(v)")
      }
    case .listTests:
      config.findTestsToRun()
      print("#\(config.delim)Test\(config.delim)[Tags]")
      for (index, t) in config.tests {
      let testDescription = [String(index), t.name, t.tags.sorted().description]
        .joined(separator: config.delim)
      print(testDescription)
      }
    case .run:
      config.findTestsToRun()
      printRunInfo(config)
      runBenchmarks(config)
      if let x = config.afterRunSleep {
        sleep(UInt32(x))
      }
    }
  } catch let error as ArgumentError {
    fflush(stdout)
    fputs(error.description, stderr)
    fflush(stderr)
    exit(1)
  } catch {
    fatalError("\(error)")
  }
}
