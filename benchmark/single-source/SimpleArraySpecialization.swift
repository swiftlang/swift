//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This benchmark tests prespecialization of a simplified array type

import TestsUtils
import SimpleArray

public let benchmarks = [
  BenchmarkInfo(
    name: "SimpleArraySpecialization",
    runFunction: run_SimpleArraySpecializationBenchmarks,
    tags: [.abstraction, .runtime, .cpubench]
  ),
  BenchmarkInfo(
    name: "SimpleArraySpecialization2",
    runFunction: run_SimpleArraySpecializationBenchmarks2,
    tags: [.abstraction, .runtime, .cpubench]
  ),
  BenchmarkInfo(
    name: "SimpleArraySpecialization3",
    runFunction: run_SimpleArraySpecializationBenchmarks3,
    tags: [.abstraction, .runtime, .cpubench]
  ),
  BenchmarkInfo(
    name: "SimpleArraySpecialization4",
    runFunction: run_SimpleArraySpecializationBenchmarks4,
    tags: [.abstraction, .runtime, .cpubench]
  ),
]

let xs = SimpleArray<MyClass>(capacity: 100_000)

@_silgen_name("_swift_stdlib_immortalize")
func _stdlib_immortalize(_ obj: AnyObject)

import Foundation


public final class MyClass {
  public var x: Int = 23
}


@inline(never)
public func run_SimpleArraySpecializationBenchmarks(_ n: Int) {
  let myObject = MyClass()

  // prevent refcount overflow
  _stdlib_immortalize(myObject)
  
  for _ in 0..<n {
    for i in 0..<100_000 {
      xs.append(myObject)
    }
    xs.clear()
  }

  blackHole(xs)
}

@inline(never)
public func run_SimpleArraySpecializationBenchmarks2(_ n: Int) {
  let myObject = MyClass()
  
  // prevent refcount overflow
  _stdlib_immortalize(myObject)

  for _ in 0..<n {
    for i in 0..<100_000 {
      xs.append2(myObject)
    }
    xs.clear()
  }

  blackHole(xs)
}

@inline(never)
public func run_SimpleArraySpecializationBenchmarks3(_ n: Int) {
  let myObject = MyClass()
  
  // prevent refcount overflow
  _stdlib_immortalize(myObject)

  for _ in 0..<n {
    for i in 0..<100_000 {
      xs.append3(myObject)
    }
    xs.clear()
  }

  blackHole(xs)
}

@inline(never)
public func run_SimpleArraySpecializationBenchmarks4(_ n: Int) {
  let myObject = MyClass()
  
  // prevent refcount overflow
  _stdlib_immortalize(myObject)

  for _ in 0..<n {
    for i in 0..<100_000 {
      xs.append4(myObject)
    }
    xs.clear()
  }

  blackHole(xs)
}
