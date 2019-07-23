//===----------------------------------------------------------------------===//
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

import Swift
import SwiftShims

public func _stdlib_getHardwareConcurrency() -> Int {
  // This is effectively a re-export of this shims function
  // for consumers of the unstable library (like unittest) to
  // use.
  return _swift_stdlib_getHardwareConcurrency()
}

/// An atomic counter for contended applications.
///
/// This is a struct to prevent extra retains/releases.  You are required to
/// call `deinit()` to release the owned memory.
public struct _stdlib_ShardedAtomicCounter {
  // Using an array causes retains/releases, which create contention on the
  // reference count.
  // FIXME: guard every element against false sharing.
  var _shardsPtr: UnsafeMutablePointer<Int>
  var _shardsCount: Int

  public init() {
    let hardwareConcurrency = _stdlib_getHardwareConcurrency()
    let count = max(8, hardwareConcurrency * hardwareConcurrency)
    let shards = UnsafeMutablePointer<Int>.allocate(capacity: count)
    for i in 0..<count {
      (shards + i).initialize(to: 0)
    }
    self._shardsPtr = shards
    self._shardsCount = count
  }

  public func `deinit`() {
    self._shardsPtr.deinitialize(count: self._shardsCount)
    self._shardsPtr.deallocate()
  }

  public func add(_ operand: Int, randomInt: Int) {
    let shardIndex = Int(UInt(bitPattern: randomInt) % UInt(self._shardsCount))
    _ = _swift_stdlib_atomicFetchAddInt(
      object: self._shardsPtr + shardIndex, operand: operand)
  }

  // FIXME: non-atomic as a whole!
  public func getSum() -> Int {
    var result = 0
    let shards = self._shardsPtr
    let count = self._shardsCount
    for i in 0..<count {
      result += _swift_stdlib_atomicLoadInt(object: shards + i)
    }
    return result
  }

  public struct PRNG {
    var _state: Int

    public init() {
      _state = Int.random(in: .min ... .max)
    }

    public mutating func randomInt() -> Int {
      var result = 0
      for _ in 0..<Int.bitWidth {
        result = (result << 1) | (_state & 1)
        _state = (_state >> 1) ^ (-(_state & 1) & Int(bitPattern: 0xD0000001))
      }
      return result
    }
  }
}
