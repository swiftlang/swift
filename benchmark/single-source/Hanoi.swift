//===--- Hanoi.swift ------------------------------------------------------===//
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

// This test checks performance of Swift hanoi tower.
// <rdar://problem/22151932>
import Foundation
import TestsUtils

struct Move {
   var from: String
   var to: String
   init(from:String, to:String) {
      self.from = from
      self.to = to
   }
}

class TowersOfHanoi {
  // Record all moves made.
  var moves : [Move] = [Move]()

  func solve(_ n: Int, start: String, auxiliary: String, end: String) {
    if (n == 1) {
      moves.append(Move(from:start, to:end))
    } else {
      solve(n - 1, start: start, auxiliary: end, end: auxiliary)
      moves.append(Move(from:start, to:end))
      solve(n - 1, start: auxiliary, auxiliary: start, end: end)
    }
  }
}

@inline(never)
public func run_Hanoi(_ N: Int) {
  for _ in 1...100*N {
    let hanoi: TowersOfHanoi = TowersOfHanoi()
    hanoi.solve(10, start: "A", auxiliary: "B", end: "C")
  }
}
