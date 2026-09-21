// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-future-triple -O -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// This test samples process usage in a Darwin specific way;
// It's enough to verify cache behavior on one platform.
// REQUIRES: OS=macosx

// This test measures RSS growth via a fixed byte threshold, which ASan invalidates rather than releasing
// UNSUPPORTED: asan

import Distributed
import FakeDistributedActorSystems

#if canImport(Darwin)
import Darwin
#endif

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter {
  distributed func hello() {}
}

// Build a well-formed distributed-thunk mangled name whose single parameter is Swift.Int
// wrapped `depth` times in Array/Optional layers selected by the bits of `pattern`.
//
// Each pattern would demangle into a distinct nested-generic parameter type.
func syntheticTargetName(pattern: Int, depth: Int) -> String {
  var inner = "Si" // Swift.Int
  for j in 0..<depth {
    if (pattern >> j) & 1 == 1 {
      inner = "Say" + inner + "G" // Array<inner>
    } else {
      inner = inner + "Sg" // Optional<inner>
    }
  }
  return "$s4main7GreeterC1f1vy" + inner + "_tYaKFTE"
}

// High-water-mark resident set size in bytes (Darwin ru_maxrss is in bytes).
func currentMaxRSSBytes() -> Int {
  var usage = rusage()
  _ = getrusage(RUSAGE_SELF, &usage)
  return Int(usage.ru_maxrss)
}

@main
struct Main {
  static func main() async {
    let system = FakeActorSystem()
    let greeter = Greeter(actorSystem: system)
    let handler = FakeRoundtripResultHandler({ _ in }, onError: { _ in })

    let iterations = 50_000
    let depth = 20

    @inline(never)
    func drive(_ name: String) async {
      var decoder = FakeInvocationDecoder(args: [], substitutions: [])
      do {
        try await system.executeDistributedTarget(
          on: greeter,
          target: RemoteCallTarget(name),
          invocationDecoder: &decoder,
          handler: handler)
      } catch {
        // Expected: every call is rejected with targetAccessorNotFound.
      }
    }

    // Phase A (control): one repeated identifier.
    let control = syntheticTargetName(pattern: 0xABCDE, depth: depth)
    let beforeControl = currentMaxRSSBytes()
    for _ in 0..<iterations {
      await drive(control)
    }
    let controlGrowth = currentMaxRSSBytes() - beforeControl

    // Phase B: many DISTINCT identifiers.
    let beforeDistinct = currentMaxRSSBytes()
    for i in 0..<iterations {
      await drive(syntheticTargetName(pattern: i, depth: depth))
    }
    let distinctGrowth = currentMaxRSSBytes() - beforeDistinct

    // Generous slack keeps this RSS-based check robust: post-fix both phases
    // early-reject and grow near zero; pre-fix phase B grows tens of MB.
    let slackBytes = 8 * 1024 * 1024
    if distinctGrowth <= controlGrowth + slackBytes {
      print("PASS: bounded control=\(controlGrowth) distinct=\(distinctGrowth)")
    } else {
      print("FAIL: unbounded control=\(controlGrowth) distinct=\(distinctGrowth)")
    }
    // CHECK: PASS: bounded
  }
}
