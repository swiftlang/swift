//===--- ProtocolDispatch2.swift ------------------------------------------===//
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
//  This is a simple benchmark that tests the performance of calls to
//  existential methods.
//===----------------------------------------------------------------------===//


import TestsUtils
import Foundation

protocol Pingable { func ping() -> Int;  func pong() -> Int}

struct Game : Pingable {
  func zero() -> Int { return 0}
  func ping() -> Int { return 1}
  func pong() -> Int { return 2}
  func puff() -> Int { return 3}
}

@inline(never)
func use_protocol(_ val : Int,_ game1 : Pingable, _ game2 : Pingable) -> Int {
  var t = game1.ping() + game1.pong()
  if (val % 2 == 0) {
    t += game1.pong() + game1.ping()
  }
  t += game1.ping() + game1.pong()

  t += game2.ping() + game2.pong()
  if (val % 2 == 0) {
    t += game2.pong() + game2.ping()
  }
  t += game2.ping() + game2.pong()

  return t
}

@inline(never)
func wrapper(_ val : Int,_ game1 : Pingable, _ game2 : Pingable) -> Int {
  return use_protocol(val, game1, game2)
}

@inline(never)
public func run_ProtocolDispatch2(_ N: Int) {
  var c = 0
  let g1 = Game()
  let g2 = Game()
  for _ in 1...N {
    c = 0
    for i in 1...5000 {
      c += wrapper(i, g1, g2)
    }
  }
  CheckResults(c == 75000, "IncorrectResults in ProtoDispatch")
}

