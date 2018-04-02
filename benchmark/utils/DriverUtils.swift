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
  var delim: String  = ","
  var sampleCount: UInt64 = 0
  var min: UInt64 = 0
  var max: UInt64 = 0
  var mean: UInt64 = 0
  var sd: UInt64 = 0
  var median: UInt64 = 0

  init() {}

  init(delim: String, sampleCount: UInt64, min: UInt64, max: UInt64, mean: UInt64, sd: UInt64, median: UInt64) {
    self.delim = delim
    self.sampleCount = sampleCount
    self.min = min
    self.max = max
    self.mean = mean
    self.sd = sd
    self.median = median

    // Sanity the bounds of our results
    precondition(self.min <= self.max, "min should always be <= max")
    precondition(self.min <= self.mean, "min should always be <= mean")
    precondition(self.min <= self.median, "min should always be <= median")
    precondition(self.max >= self.mean, "max should always be >= mean")
    precondition(self.max >= self.median, "max should always be >= median")
  }
}

extension BenchResults : CustomStringConvertible {
  var description: String {
     return "\(sampleCount)\(delim)\(min)\(delim)\(max)\(delim)\(mean)\(delim)\(sd)\(delim)\(median)"
  }
}

struct Test {
  let benchInfo: BenchmarkInfo
  let index: Int

  /// The name of the benchmark.
  var name: String {
    return benchInfo.name
  }

  /// The "main routine" of the benchmark.
  var runFunction: (Int) -> () {
    return benchInfo.runFunction
  }

  /// The benchmark categories that this test belongs to. Used for filtering.
  var tags: [BenchmarkCategory] {
    return benchInfo.tags
  }

  /// An optional initialization function for a benchmark that is run before
  /// measuring begins. Intended to be used to initialize global data used in
  /// a benchmark.
  var setUpFunction: (() -> ())? {
    return benchInfo.setUpFunction
  }

  /// An optional deinitialization function that if non-null is run /after/ a
  /// measurement has been taken.
  var tearDownFunction: (() -> ())? {
    return benchInfo.tearDownFunction
  }
}

// We should migrate to a collection of BenchmarkInfo.
public var registeredBenchmarks: [BenchmarkInfo] = []

enum TestAction {
  case run
  case listTests
  case fail(String)
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
  var tests = [Test]()

  mutating func processArguments() -> TestAction {
    let validOptions = [
      "--iter-scale", "--num-samples", "--num-iters",
      "--verbose", "--delim", "--list", "--sleep",
      "--tags", "--skip-tags"
    ]
    let maybeBenchArgs: Arguments? = parseArgs(validOptions)
    if maybeBenchArgs == nil {
      return .fail("Failed to parse arguments")
    }
    let benchArgs = maybeBenchArgs!

    filters = benchArgs.positionalArgs

    if let x = benchArgs.optionalArgsMap["--iter-scale"] {
      if x.isEmpty { return .fail("--iter-scale requires a value") }
      iterationScale = Int(x)!
    }

    if let x = benchArgs.optionalArgsMap["--num-iters"] {
      if x.isEmpty { return .fail("--num-iters requires a value") }
      fixedNumIters = numericCast(Int(x)!)
    }

    if let x = benchArgs.optionalArgsMap["--num-samples"] {
      if x.isEmpty { return .fail("--num-samples requires a value") }
      numSamples = Int(x)!
    }

    if let _ = benchArgs.optionalArgsMap["--verbose"] {
      verbose = true
      print("Verbose")
    }

    if let x = benchArgs.optionalArgsMap["--delim"] {
      if x.isEmpty { return .fail("--delim requires a value") }
      delim = x
    }

    if let x = benchArgs.optionalArgsMap["--tags"] {
      if x.isEmpty { return .fail("--tags requires a value") }

      // We support specifying multiple tags by splitting on comma, i.e.:
      //
      //  --tags=array,set
      //
      // FIXME: If we used Error instead of .fail, then we could have a cleaner
      // impl here using map on x and tags.formUnion.
      for t in x.split(separator: ",") {
        guard let cat = BenchmarkCategory(rawValue: String(t)) else {
          return .fail("Unknown benchmark category: '\(t)'")
        }
        tags.insert(cat)
      }
    }

    if let x = benchArgs.optionalArgsMap["--skip-tags"] {
      // if the --skip-tags parameter is specified, we need to ignore the
      // default and start from a clean slate.
      skipTags = []

      // We support specifying multiple tags by splitting on comma, i.e.:
      //
      //  --skip-tags=array,set
      //
      // FIXME: If we used Error instead of .fail, then we could have a cleaner
      // impl here using map on x and tags.formUnion.
      for t in x.split(separator: ",") {
        guard let cat = BenchmarkCategory(rawValue: String(t)) else {
          return .fail("Unknown benchmark category: '\(t)'")
        }
        skipTags.insert(cat)
      }
    }

    if let x = benchArgs.optionalArgsMap["--sleep"] {
      guard let v = Int(x) else {
        return .fail("--sleep requires a non-empty integer value")
      }
      afterRunSleep = v
    }

    if let _ = benchArgs.optionalArgsMap["--list"] {
      return .listTests
    }

    return .run
  }

  mutating func findTestsToRun() {
    let benchmarkNameFilter = Set(filters)

    // t is needed so we don't capture an ivar of a mutable inout self.
    let t = tags
    let st = skipTags
    let filteredTests = Array(registeredBenchmarks.filter { benchInfo in
      if !t.isSubset(of: benchInfo.tags) {
        return false
      }

      if !st.isDisjoint(with: benchInfo.tags) {
        return false
      }

      // If the user did not specified a benchmark name filter and our tags are
      // a subset of the specified tags by the user, return true. We want to run
      // this test.
      if benchmarkNameFilter.isEmpty {
        return true
      }

      // Otherwise, we need to check if our benchInfo's name is in the benchmark
      // name filter list. If it isn't, then we shouldn't process it.
      return benchmarkNameFilter.contains(benchInfo.name)
    }).sorted()

    if (filteredTests.isEmpty) {
      return
    }

    tests = filteredTests.enumerated().map {
      Test(benchInfo: $0.element, index: $0.offset + 1)
    }
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
func runBench(_ test: Test, _ c: TestConfig) -> BenchResults {
  var samples = [UInt64](repeating: 0, count: c.numSamples)

  if c.verbose {
    print("Running \(test.name) for \(c.numSamples) samples.")
  }

  let sampler = SampleRunner()
  for s in 0..<c.numSamples {
    let time_per_sample: UInt64 = 1_000_000_000 * UInt64(c.iterationScale)

    var scale : UInt
    var elapsed_time : UInt64 = 0
    if c.fixedNumIters == 0 {
      test.setUpFunction?()
      elapsed_time = sampler.run(test.name, fn: test.runFunction, num_iters: 1)
      test.tearDownFunction?()

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
    }

    // Rerun the test with the computed scale factor.
    if scale > 1 {
      if c.verbose {
        print("    Measuring with scale \(scale).")
      }
      test.setUpFunction?()
      elapsed_time = sampler.run(test.name, fn: test.runFunction, num_iters: scale)
      test.tearDownFunction?()
    } else {
      scale = 1
    }
    // save result in microseconds or k-ticks
    samples[s] = elapsed_time / UInt64(scale) / 1000
    if c.verbose {
      print("    Sample \(s),\(samples[s])")
    }
  }

  let (mean, sd) = internalMeanSD(samples)

  // Return our benchmark results.
  return BenchResults(delim: c.delim, sampleCount: UInt64(samples.count),
                      min: samples.min()!, max: samples.max()!,
                      mean: mean, sd: sd, median: internalMedian(samples))
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
    for t in c.tests {
      print("\(t.name), ", terminator: "")
    }
    print("")
    print("")
    print("--- DATA ---")
  }
}

func runBenchmarks(_ c: TestConfig) {
  let units = "us"
  print("#\(c.delim)TEST\(c.delim)SAMPLES\(c.delim)MIN(\(units))\(c.delim)MAX(\(units))\(c.delim)MEAN(\(units))\(c.delim)SD(\(units))\(c.delim)MEDIAN(\(units))")
  var sumBenchResults = BenchResults()
  sumBenchResults.sampleCount = 0

  for t in c.tests {
    let results = runBench(t, c)
    print("\(t.index)\(c.delim)\(t.name)\(c.delim)\(results.description)")
    fflush(stdout)

    sumBenchResults.min += results.min
    sumBenchResults.max += results.max
    sumBenchResults.mean += results.mean
    sumBenchResults.sampleCount += 1
    // Don't accumulate SD and Median, as simple sum isn't valid for them.
    // TODO: Compute SD and Median for total results as well.
    // sumBenchResults.sd += results.sd
    // sumBenchResults.median += results.median
  }

  print("")
  print("Totals\(c.delim)\(sumBenchResults.description)")
}

public func main() {
  var config = TestConfig()

  switch (config.processArguments()) {
    case let .fail(msg):
      // We do this since we need an autoclosure...
      fatalError("\(msg)")
    case .listTests:
      config.findTestsToRun()
      print("Enabled Tests\(config.delim)Tags")
      for t in config.tests {
        print("\(t.name)\(config.delim)\(t.tags)")
      }
    case .run:
      config.findTestsToRun()
      printRunInfo(config)
      runBenchmarks(config)
      if let x = config.afterRunSleep {
        sleep(UInt32(x))
      }
  }
}
