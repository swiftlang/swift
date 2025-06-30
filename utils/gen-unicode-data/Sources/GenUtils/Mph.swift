//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public struct Mph {
  public var bitArrays: [BitArray] = []
  public var ranks: [[UInt16]] = []

  init(gamma: Double, keys: [UInt64]) {
    var size: Int
    var a: BitArray
    var collide: Set<Int>
    var redoKeys: [UInt64] = keys
    var i: UInt64 = 0

    repeat {
      size = Swift.max(64, Int(gamma * Double(redoKeys.count)))
      a = BitArray(size: size)
      collide = []

      for key in redoKeys {
        let idx = Int(hash(key, UInt64(size), seed: i))

        if !collide.contains(idx), !a.insert(idx) {
          collide.insert(idx)
        }
      }

      var tmpRedo: [UInt64] = []

      for key in redoKeys {
        let idx = Int(hash(key, UInt64(size), seed: i))

        if collide.contains(idx) {
          a[idx] = false
          tmpRedo.append(key)
        }
      }

      bitArrays.append(a)
      redoKeys = tmpRedo
      i += 1
    } while !redoKeys.isEmpty

    computeRanks()
  }

  mutating func computeRanks() {
    var pop: UInt16 = 0

    for bitArray in bitArrays {
      var rank: [UInt16] = []

      for i in 0 ..< bitArray.words.count {
        let v = bitArray.words[i]

        if i % 8 == 0 {
          rank.append(pop)
        }

        pop += UInt16(v.nonzeroBitCount)
      }

      ranks.append(rank)
    }
  }

  public func index(for key: UInt64) -> Int {
    for i in 0 ..< bitArrays.count {
      let b = bitArrays[i]
      let idx = Int(hash(key, UInt64(b.size), seed: UInt64(i)))

      if b[idx] {
        var rank = ranks[i][idx / 512]

        for j in (idx / 64) & ~7 ..< idx / 64 {
          rank += UInt16(b.words[j].nonzeroBitCount)
        }

        let finalWord = b.words[idx / 64]

        if idx % 64 > 0 {
          rank += UInt16((finalWord << (64 - (idx % 64))).nonzeroBitCount)
        }

        return Int(rank)
      }
    }

    return -1
  }
}

public func mph(for keys: [UInt64]) -> Mph {
  Mph(gamma: 1, keys: keys)
}
