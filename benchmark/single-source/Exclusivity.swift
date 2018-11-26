//===--- Exclusivity.swift -------------------------------------------------===//
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
//
// A set of tests for measuring the enforcement overhead of memory access
// exclusivity rules.
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let Exclusivity = [
  // At -Onone
  // 25% swift_beginAccess
  // 15% tlv_get_addr
  // 15% swift_endAccess
  BenchmarkInfo(
    name: "ExclusivityGlobal",
    runFunction: run_accessGlobal,
    tags: [.runtime, .cpubench]
  ),
  // At -Onone
  // 23% swift_retain
  // 22% swift_release
  //  9% swift_beginAccess
  //  3% swift_endAccess
  BenchmarkInfo(
    name: "ExclusivityInMatSet",
    runFunction: run_accessInMatSet,
    tags: [.runtime, .cpubench, .unstable]
  ),
  // At -Onone
  // 25% swift_release
  // 23% swift_retain
  // 16% swift_beginAccess
  //  8% swift_endAccess
  BenchmarkInfo(
    name: "ExclusivityIndependent",
    runFunction: run_accessIndependent,
    tags: [.runtime, .cpubench]
  ),
]

// Initially these benchmarks only measure access checks at -Onone. In
// the future, access checks will also be emitted at -O.

// Measure memory access checks on a trivial global.
// ---

public var globalCounter: Int = 0

// TODO:
// - Merge begin/endAccess when no calls intervene (~2x speedup).
// - Move Swift runtime into the OS  (~2x speedup).
// - Whole module analysis can remove exclusivity checks (> 10x speedup now, 4x speedup with runtime in OS).
//   (The global's "public" qualifier should make the benchmark immune to this optimization.)
@inline(never)
public func run_accessGlobal(_ N: Int) {
  globalCounter = 0
  for _ in 1...10000*N {
    globalCounter += 1
  }
  CheckResults(globalCounter == 10000*N)
}

// Measure memory access checks on a class property.
//
// Note: The end_unpaired_access forces a callback on the property's
// materializeForSet!
// ---

// Hopefully the optimizer will not see this as "final" and optimize away the
// materializeForSet.
public class C {
  public var counter = 0

  func inc() {
    counter += 1
  }
}

// Thunk 
@inline(never)
func updateClass(_ c: C) {
  c.inc()
}

// TODO: Replacing materializeForSet accessors with yield-once
// accessors should make the callback overhead go away.
@inline(never)
public func run_accessInMatSet(_ N: Int) {
  let c = C()
  for _ in 1...10000*N {
    updateClass(c)
  }
  CheckResults(c.counter == 10000*N)
}

// Measure nested access to independent objects.
//
// A single access set is still faster than hashing for up to four accesses.
// ---

struct Var {
  var val = 0
}

@inline(never)
func update(a: inout Var, b: inout Var, c: inout Var, d: inout Var) {
  a.val += 1
  b.val += 1
  c.val += 1
  d.val += 1
}

@inline(never)
public func run_accessIndependent(_ N: Int) {
  var a = Var()
  var b = Var()
  var c = Var()
  var d = Var()
  let updateVars = {
    update(a: &a, b: &b, c: &c, d: &d)
  }
  for _ in 1...1000*N {
    updateVars()
  }
  CheckResults(a.val == 1000*N && b.val == 1000*N && c.val == 1000*N
    && d.val == 1000*N)
}
