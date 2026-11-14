// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -target %target-future-triple -O -j2 -parse-as-library %s %S/../Inputs/SilentRoundtripActorSystem.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// This test reads the malloc zones' statistics in a Darwin specific way;
// It's enough to verify cache behavior on one platform.
// REQUIRES: OS=macosx

// This test measures the growth of the bytes in use via a fixed byte threshold, which ASan invalidates rather than releasing
// UNSUPPORTED: asan

// The runtime keeps one resolved signature per distributed target, so calling
// the same targets many times, generic and not, must not grow the bytes malloc
// has in use. The arities and the iteration count make a parameter type buffer
// leaked per call visible: phase A makes 200,000 calls each to a two-parameter
// target (a 16 byte buffer) and an eight-parameter one (64 bytes), 16 MB if
// leaked; phase B makes 400,000 calls to a four-parameter generic target
// (32 bytes), 12.8 MB if leaked; the allowance is 4 MiB per phase, after a
// warm-up. Unknown names are rejected before the cache is reached, which
// distributed_executeDistributedTarget_metadata_growth.swift checks.

import Distributed

#if canImport(Darwin)
import Darwin
#endif

typealias DefaultDistributedActorSystem = SilentRoundtripActorSystem

distributed actor Worker {
  distributed func twoInts(_ a: Int, _ b: Int) -> Int { a &+ b }
  distributed func eightValues(
    _ s: String, _ values: [Int], _ flag: Bool, _ d: Double,
    _ a: Int, _ b: Int, _ c: Int, _ e: Int
  ) -> String {
    "\(s):\(values.count):\(flag):\(d):\(a &+ b &+ c &+ e)"
  }
  distributed func generic<T: Codable & Sendable>(
    _ value: T, _ count: Int, _ tag: String, _ flag: Bool
  ) -> [T] {
    flag ? Array(repeating: value, count: count) : []
  }
}

// Bytes currently allocated across all malloc zones. Unlike a resident set
// high-water mark this falls when memory is freed, so a leak per call shows as
// growth even after an earlier peak.
func mallocBytesInUse() -> Int {
  var statistics = malloc_statistics_t()
  malloc_zone_statistics(nil, &statistics)
  return Int(statistics.size_in_use)
}

@main
struct Main {
  static func main() async throws {
    let system = DefaultDistributedActorSystem()
    let worker = Worker(actorSystem: system)
    let ref = try Worker.resolve(id: worker.id, using: system)

    let iterations = 200_000
    let slackBytes = 4 * 1024 * 1024

    // Warm up, so that the phases measure steady-state calls only.
    for i in 0..<100 {
      _ = try await ref.twoInts(i, 2)
      _ = try await ref.eightValues("m", [1, 2], true, 0.5, i, 1, 2, 3)
      _ = try await ref.generic(i, 2, "t", true)
      _ = try await ref.generic("g", 1, "t", true)
    }

    // Phase A: the same non-generic targets, repeatedly.
    var mismatches = 0
    let beforeRepeated = mallocBytesInUse()
    for i in 0..<iterations {
      if try await ref.twoInts(i, 2) != i + 2 { mismatches += 1 }
      if try await ref.eightValues("m", [1, 2], true, 0.5, i, 1, 2, 3) != "m:2:true:0.5:\(i + 6)" {
        mismatches += 1
      }
    }
    let repeatedGrowth = mallocBytesInUse() - beforeRepeated
    // CHECK: repeated: mismatches 0
    print("repeated: mismatches \(mismatches)")

    // Phase B: the same generic target, repeatedly, with two substitutions.
    mismatches = 0
    let beforeGeneric = mallocBytesInUse()
    for i in 0..<iterations {
      if try await ref.generic(i, 2, "t", true) != [i, i] { mismatches += 1 }
      if try await ref.generic("g", 1, "t", true) != ["g"] { mismatches += 1 }
    }
    let genericGrowth = mallocBytesInUse() - beforeGeneric
    // CHECK: generic: mismatches 0
    print("generic: mismatches \(mismatches)")

    // CHECK: PASS: bounded
    if repeatedGrowth <= slackBytes && genericGrowth <= slackBytes {
      print("PASS: bounded repeated=\(repeatedGrowth) generic=\(genericGrowth)")
    } else {
      print("FAIL: unbounded repeated=\(repeatedGrowth) generic=\(genericGrowth)")
    }
  }
}
