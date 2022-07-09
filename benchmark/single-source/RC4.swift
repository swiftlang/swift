//===--- RC4.swift --------------------------------------------------------===//
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

// This test is based on util/benchmarks/RC4, with modifications
// for performance measuring.
import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "RC4",
    runFunction: run_RC4,
    tags: [.validation, .algorithm])

struct RC4 {
  var state: [UInt8]
  var i: UInt8 = 0
  var j: UInt8 = 0

  init() {
    state = [UInt8](repeating: 0, count: 256)
  }

  mutating
  func initialize(_ key: [UInt8]) {
    for i in 0..<256 {
      state[i] = UInt8(i)
    }

    var j: UInt8 = 0
    for i in 0..<256 {
      let k: UInt8 = key[i % key.count]
      let s: UInt8 = state[i]
      j = j &+ s &+ k
      swapByIndex(i, y: Int(j))
    }
  }

  mutating
  func swapByIndex(_ x: Int, y: Int) {
    let t1: UInt8 = state[x]
    let t2: UInt8 = state[y]
    state[x] = t2
    state[y] = t1
  }

  mutating
  func next() -> UInt8 {
    i = i &+ 1
    j = j &+ state[Int(i)]
    swapByIndex(Int(i), y: Int(j))
    return state[Int(state[Int(i)] &+ state[Int(j)]) & 0xFF]
  }

  mutating
  func encrypt(_ data: inout [UInt8]) {
    let cnt = data.count
    for i in 0..<cnt {
      data[i] = data[i] ^ next()
    }
  }
}

let refResults: [UInt8] = [
  245, 62, 245, 202, 138, 120, 186, 107, 255, 189,
  184, 223, 65, 77, 112, 201, 238, 161, 74, 192, 145,
  21, 43, 41, 91, 136, 182, 176, 237, 155, 208, 16,
  17, 139, 33, 195, 24, 136, 79, 183, 211, 21, 56,
  202, 235, 65, 201, 184, 68, 29, 110, 218, 112, 122,
  194, 77, 41, 230, 147, 84, 0, 233, 168, 6, 55, 131,
  70, 119, 41, 119, 234, 131, 87, 24, 51, 130, 28,
  66, 172, 105, 33, 97, 179, 48, 81, 229, 114, 216,
  208, 119, 39, 31, 47, 109, 172, 215, 246, 210, 48,
  203]


@inline(never)
public func run_RC4(_ n: Int) {
  let messageLen = 100
  let iterations = 500
  let secret = "This is my secret message"
  let key    = "This is my key"
  let secretData : [UInt8] = Array(secret.utf8)
  let keyData    : [UInt8] = Array(key.utf8)

  var longData : [UInt8] = [UInt8](repeating: 0, count: messageLen)

  for _ in 1...n {
    // Generate a long message.
    for i in 0..<messageLen {
      longData[i] = secretData[i % secretData.count]
    }

    var enc = RC4()
    enc.initialize(keyData)

    for _ in 1...iterations {
      enc.encrypt(&longData)
    }

    check(longData == refResults)
  }
}
