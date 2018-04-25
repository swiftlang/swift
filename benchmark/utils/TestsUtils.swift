//===--- TestsUtils.swift -------------------------------------------------===//
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

public enum BenchmarkCategory : String {
  // Validation "micro" benchmarks test a specific operation or critical path that
  // we know is important to measure.
  case validation
  // subsystems to validate and their subcategories.
  case api, Array, String, Dictionary, Codable, Set, Data
  case sdk
  case runtime, refcount, metadata
  // Other general areas of compiled code validation.
  case abstraction, safetychecks, exceptions, bridging, concurrency
   
  // Algorithms are "micro" that test some well-known algorithm in isolation:
  // sorting, searching, hashing, fibonaci, crypto, etc.
  case algorithm
   
  // Miniapplications are contrived to mimic some subset of application behavior
  // in a way that can be easily measured. They are larger than micro-benchmarks,
  // combining multiple APIs, data structures, or algorithms. This includes small
  // standardized benchmarks, pieces of real applications that have been extracted
  // into a benchmark, important functionality like JSON parsing, etc.
  case miniapplication
   
  // Regression benchmarks is a catch-all for less important "micro"
  // benchmarks. This could be a random piece of code that was attached to a bug
  // report. We want to make sure the optimizer as a whole continues to handle
  // this case, but don't know how applicable it is to general Swift performance
  // relative to the other micro-benchmarks. In particular, these aren't weighted
  // as highly as "validation" benchmarks and likely won't be the subject of
  // future investigation unless they significantly regress.
  case regression
   
  // Most benchmarks are assumed to be "stable" and will be regularly tracked at
  // each commit. A handful may be marked unstable if continually tracking them is
  // counterproductive.
  case unstable
   
  // CPU benchmarks represent instrinsic Swift performance. They are useful for
  // measuring a fully baked Swift implementation across different platforms and
  // hardware. The benchmark should also be reasonably applicable to real Swift
  // code--it should exercise a known performance critical area. Typically these
  // will be drawn from the validation benchmarks once the language and standard
  // library implementation of the benchmark meets a reasonable efficiency
  // baseline. A benchmark should only be tagged "cpubench" after a full
  // performance investigation of the benchmark has been completed to determine
  // that it is a good representation of future Swift performance. Benchmarks
  // should not be tagged if they make use of an API that we plan on
  // reimplementing or call into code paths that have known opportunities for
  // significant optimization.
  case cpubench

  // Explicit skip marker
  case skip
}

public struct BenchmarkInfo {
  /// The name of the benchmark that should be displayed by the harness.
  public var name: String

  /// A function that invokes the specific benchmark routine.
  public var runFunction: (Int) -> ()

  /// A set of category tags that describe this benchmark. This is used by the
  /// harness to allow for easy slicing of the set of benchmarks along tag
  /// boundaries, e.x.: run all string benchmarks or ref count benchmarks, etc.
  public var tags: [BenchmarkCategory]

  /// An optional function that if non-null is run before benchmark samples
  /// are timed.
  public var setUpFunction: (() -> ())?

  /// An optional function that if non-null is run immediately after a sample is
  /// taken.
  public var tearDownFunction: (() -> ())?

  public init(name: String, runFunction: @escaping (Int) -> (), tags: [BenchmarkCategory],
              setUpFunction: (() -> ())? = nil,
              tearDownFunction: (() -> ())? = nil) {
    self.name = name
    self.runFunction = runFunction
    self.tags = tags
    self.setUpFunction = setUpFunction
    self.tearDownFunction = tearDownFunction
  }
}

extension BenchmarkInfo : Comparable {
  public static func < (lhs: BenchmarkInfo, rhs: BenchmarkInfo) -> Bool {
    return lhs.name < rhs.name
  }
  public static func == (lhs: BenchmarkInfo, rhs: BenchmarkInfo) -> Bool {
    return lhs.name == rhs.name
  }
}

extension BenchmarkInfo : Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(name)
  }
}

// Linear function shift register.
//
// This is just to drive benchmarks. I don't make any claim about its
// strength. According to Wikipedia, it has the maximal period for a
// 32-bit register.
struct LFSR {
  // Set the register to some seed that I pulled out of a hat.
  var lfsr : UInt32 = 0xb78978e7

  mutating func shift() {
    lfsr = (lfsr >> 1) ^ (UInt32(bitPattern: -Int32((lfsr & 1))) & 0xD0000001)
  }
  mutating func randInt() -> Int64 {
    var result : UInt32 = 0
    for _ in 0..<32 {
      result = (result << 1) | (lfsr & 1)
      shift()
    }
    return Int64(bitPattern: UInt64(result))
  }
}

var lfsrRandomGenerator = LFSR()

// Start the generator from the beginning
public func SRand() {
  lfsrRandomGenerator = LFSR()
}

public func Random() -> Int64 {
  return lfsrRandomGenerator.randInt()
}

@inline(__always)
public func CheckResults(
    _ resultsMatch: Bool,
    file: StaticString = #file,
    function: StaticString = #function,
    line: Int = #line
    ) {
    guard _fastPath(resultsMatch) else {
        print("Incorrect result in \(function), \(file):\(line)")
        abort()
    }
}

public func False() -> Bool { return false }

/// This is a dummy protocol to test the speed of our protocol dispatch.
public protocol SomeProtocol { func getValue() -> Int }
struct MyStruct : SomeProtocol {
  init() {}
  func getValue() -> Int { return 1 }
}
public func someProtocolFactory() -> SomeProtocol { return MyStruct() }

// Just consume the argument.
// It's important that this function is in another module than the tests
// which are using it.
@inline(never)
public func blackHole<T>(_ x: T) {
}

// Return the passed argument without letting the optimizer know that.
@inline(never)
public func identity<T>(_ x: T) -> T {
  return x
}

// Return the passed argument without letting the optimizer know that.
// It's important that this function is in another module than the tests
// which are using it.
@inline(never)
public func getInt(_ x: Int) -> Int { return x }

// The same for String.
@inline(never)
public func getString(_ s: String) -> String { return s }
