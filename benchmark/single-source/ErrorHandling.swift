//===--- ErrorHandling.swift ----------------------------------------------===//
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

import TestsUtils

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
public func run_ErrorHandling(_ N: Int) {
  for _ in 1...5000*N {
    do {
      _ = try doSomething()
    } catch _ {

    }
  }
}

