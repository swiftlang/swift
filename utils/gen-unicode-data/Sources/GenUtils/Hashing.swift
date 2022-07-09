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

func hash(_ key: UInt64, _ n: UInt64, seed: UInt64) -> UInt64 {
  let key = key | (n << 32)
  let hash = UInt64(murmur3(key, seed: UInt32(seed)))
  
  return hash % n
}

func scramble(_ key: UInt32) -> UInt32 {
  var key = key
  key &*= 0xCC9E2D51
  key = (key << 15) | (key >> 17)
  key &*= 0x1B873593
  return key
}

func murmur3(_ key: UInt64, seed: UInt32) -> UInt32 {
  var hash = seed
  var k: UInt32
  var key = key
  
  for _ in 0 ..< 2 {
    k = UInt32((key << 32) >> 32)
    key >>= 32
    
    hash ^= scramble(k)
    hash = (hash << 13) | (hash >> 19)
    hash = hash &* 5 &+ 0xE6546B64
  }
  
  hash ^= 8
  hash ^= hash >> 16
  hash &*= 0x85EBCA6B
  hash ^= hash >> 13
  hash &*= 0xC2B2AE35
  hash ^= hash >> 16
  
  return hash
}
