//===--- RC4.swift --------------------------------------------------------===//
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

// This test is based on util/benchmarks/RC4, with modifications
// for performance measuring.
import TestsUtils

struct RC4 {
  var State : [UInt8]
  var I: UInt8 = 0
  var J: UInt8 = 0

  init() {
    State = [UInt8](repeating: 0, count: 256)
  }

  mutating
  func initialize(_ Key: [UInt8]) {
    for i in 0..<256 {
      State[i] = UInt8(i)
    }

    var j: UInt8 = 0
    for i in 0..<256 {
      let K : UInt8 = Key[i % Key.count]
      let S : UInt8 = State[i]
      j = j &+ S &+ K
      swapByIndex(i, y: Int(j))
    }
  }

  mutating
  func swapByIndex(_ x: Int, y: Int) {
    let T1 : UInt8 = State[x]
    let T2 : UInt8 = State[y]
    State[x] = T2
    State[y] = T1
  }

  mutating
  func next() -> UInt8 {
    I = I &+ 1
    J = J &+ State[Int(I)]
    swapByIndex(Int(I), y: Int(J))
    return State[Int(State[Int(I)] &+ State[Int(J)]) & 0xFF]
  }

  mutating
  func encrypt(_ Data: inout [UInt8]) {
    let cnt = Data.count
    for i in 0..<cnt {
      Data[i] = Data[i] ^ next()
    }
  }
}

let RefResults : [UInt8] = [245, 62, 245, 202, 138, 120, 186, 107, 255, 189,
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
public func run_RC4(_ N: Int) {
  let messageLen = 100
  let iterations = 500
  let Secret = "This is my secret message"
  let Key    = "This is my key"
  let SecretData : [UInt8] = Array(Secret.utf8)
  let KeyData    : [UInt8] = Array(Key.utf8)

  var LongData : [UInt8] = [UInt8](repeating: 0, count: messageLen)

  for _ in 1...N {
    // Generate a long message.
    for i in 0..<messageLen {
      LongData[i] = SecretData[i % SecretData.count]
    }

    var Enc = RC4()
    Enc.initialize(KeyData)

    for _ in 1...iterations {
      Enc.encrypt(&LongData)
    }

    CheckResults(LongData == RefResults, "Incorrect result in RC4")
  }
}
