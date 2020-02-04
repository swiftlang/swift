//===--- DevirtualizeProtocolComposition.swift -------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let DevirtualizeProtocolComposition = [
  BenchmarkInfo(name: "DevirtualizeProtocolComposition", runFunction: run_DevirtualizeProtocolComposition, tags: [.validation, .api]),
]

public class ClassA<T> { }

protocol ProtocolA {
  func foo() -> Int
}

protocol ProtocolB {
  func bar() -> Int
}

public class ClassB: ClassA<String> {
  func foo() -> Int {
    return 10
  }
}

extension ClassB: ProtocolA { }

func quadratic(a: Int) -> Int {
  var sum = 0
  for _ in 0..<a {
    for _ in 0..<a {
      sum += 1
    }
  }
  return sum
}

func shouldOptimize1<T>(_ x: ClassA<T> & ProtocolA, count: Int) -> Int {
  var sum = 0
  for _ in 0..<count {
    sum += quadratic(a: x.foo())
  }
  return sum
}

@inline(never)
public func entryPoint1(c: ClassB) -> Int {
  return shouldOptimize1(c, count: 25)
}

@inline(never)
public func run_DevirtualizeProtocolComposition(N: Int) {
  for _ in 0..<N * 20_000 {
    blackHole(entryPoint1(c: ClassB()))
  }
}
