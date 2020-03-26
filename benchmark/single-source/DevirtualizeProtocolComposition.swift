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

protocol Pingable { func ping() -> Int; func pong() -> Int}

public class Game<T> {
  func length() -> Int { return 10 }
}

public class PingPong: Game<String> { }

extension PingPong : Pingable {
  func ping() -> Int { return 1 }
  func pong() -> Int { return 2 }
}

func playGame<T>(_ x: Game<T> & Pingable) -> Int {
  var sum = 0
  for _ in 0..<x.length() {
    sum += x.ping() + x.pong()
  }
  return sum
}

@inline(never)
public func run_DevirtualizeProtocolComposition(N: Int) {
  for _ in 0..<N * 20_000 {
    blackHole(playGame(PingPong()))
  }
}
