//===--- ErrorHandling.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "ErrorHandling",
    runFunction: run_ErrorHandling,
    tags: [.validation, .exceptions],
    legacyFactor: 10)

enum PizzaError : Error {
  case Pepperoni, Olives, Anchovy
}

@inline(never)
func doSomething() throws -> String {
  var sb = "pi"
  for str in ["z","z","a","?","?","?"] {
    sb += str
    if sb == "pizza" {
      throw PizzaError.Anchovy
    }
  }
  return sb
}

@inline(never)
public func run_ErrorHandling(_ n: Int) {
  for _ in 1...500*n {
    do {
      _ = try doSomething()
    } catch _ {

    }
  }
}
