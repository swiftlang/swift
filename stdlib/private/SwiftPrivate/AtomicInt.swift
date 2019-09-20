//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

// This type intentionally shadows the stdlib one
@available(swift, introduced: 5.0)
public final class _stdlib_AtomicInt {
  internal var _valueStorage: Int

  // FIXME: This should be an UnsafeAtomicInt, but we don't want to constrain
  // availability.
  internal var _valuePtr: UnsafeMutableRawPointer {
    return _getUnsafePointerToStoredProperties(self)
  }

  public init(_ value: Int = 0) {
    _valueStorage = value
  }

  public func store(_ desired: Int) {
    _valuePtr._atomicStoreWord(UInt(bitPattern: desired))
  }

  public func load() -> Int {
    return Int(bitPattern: _valuePtr._atomicLoadWord())
  }

  @discardableResult
  public func fetchAndAdd(_ operand: Int) -> Int {
    let word = _valuePtr._atomicFetchThenWrappingAddWord(
      UInt(bitPattern: operand))
    return Int(bitPattern: word)
  }
  @discardableResult
  public func fetchAndAnd(_ operand: Int) -> Int {
    let word = _valuePtr._atomicFetchThenAndWord(UInt(bitPattern: operand))
    return Int(bitPattern: word)
  }
  @discardableResult
  public func fetchAndOr(_ operand: Int) -> Int {
    let word = _valuePtr._atomicFetchThenOrWord(UInt(bitPattern: operand))
    return Int(bitPattern: word)
  }
  @discardableResult
  public func fetchAndXor(_ operand: Int) -> Int {
    let word = _valuePtr._atomicFetchThenXorWord(UInt(bitPattern: operand))
    return Int(bitPattern: word)
  }

  public func addAndFetch(_ operand: Int) -> Int {
    return fetchAndAdd(operand) + operand
  }
  public func andAndFetch(_ operand: Int) -> Int {
    return fetchAndAnd(operand) & operand
  }
  public func orAndFetch(_ operand: Int) -> Int {
    return fetchAndOr(operand) | operand
  }
  public func xorAndFetch(_ operand: Int) -> Int {
    return fetchAndXor(operand) ^ operand
  }

  public func compareExchange(expected: inout Int, desired: Int) -> Bool {
    var expectedWord = UInt(bitPattern: expected)
    let success = _valuePtr._atomicCompareExchangeWord(
      expected: &expectedWord,
      desired: UInt(bitPattern: desired))
    expected = Int(bitPattern: expectedWord)
    return success
  }
}

