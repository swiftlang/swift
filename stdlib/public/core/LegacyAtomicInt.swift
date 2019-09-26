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

// NOTE: older runtimes had Swift._stdlib_AtomicInt as the ObjC name.
// The two must coexist, so it was renamed. The old name must not be
// used in the new runtime. _TtCs18__stdlib_AtomicInt is the mangled
// name for Swift.__stdlib_AtomicInt
@available(swift, deprecated: 4.2, obsoleted: 5.0)
@_objcRuntimeName(_TtCs18__stdlib_AtomicInt)
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
    let word = _valuePtr._atomicFetchThenAndWord(
      UInt(bitPattern: operand))
    return Int(bitPattern: word)
  }
  @discardableResult
  public func fetchAndOr(_ operand: Int) -> Int {
    let word = _valuePtr._atomicFetchThenOrWord(
      UInt(bitPattern: operand))
    return Int(bitPattern: word)
  }
  @discardableResult
  public func fetchAndXor(_ operand: Int) -> Int {
    let word = _valuePtr._atomicFetchThenXorWord(
      UInt(bitPattern: operand))
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

@available(*, deprecated)
@available(macOS, deprecated: 9999)
@available(iOS, deprecated: 9999)
@available(watchOS, deprecated: 9999)
@available(tvOS, deprecated: 9999)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicCompareExchangeStrongInt(
  object target: UnsafeMutablePointer<Int>,
  expected: UnsafeMutablePointer<Int>,
  desired: Int
) -> Bool {
  var expectedWord = UInt(bitPattern: expected.pointee)
  let success = UnsafeMutableRawPointer(target)
    ._atomicCompareExchangeWord(
      expected: &expectedWord,
      desired: UInt(bitPattern: desired))
  expected.pointee = Int(bitPattern: expectedWord)
  return success
}


// FIXME: ideally it should not be here, at the very least not public, but
// @usableFromInline internal to be used by SwiftPrivate._stdlib_AtomicInt
@available(*, deprecated)
@available(macOS, deprecated: 9999)
@available(iOS, deprecated: 9999)
@available(watchOS, deprecated: 9999)
@available(tvOS, deprecated: 9999)
public // Existing uses outside stdlib
func _swift_stdlib_atomicLoadInt(
  object target: UnsafeMutablePointer<Int>
) -> Int {
  let result = UnsafeMutableRawPointer(target)._atomicLoadWord()
  return Int(bitPattern: result)
}

@available(*, deprecated)
@available(macOS, deprecated: 9999)
@available(iOS, deprecated: 9999)
@available(watchOS, deprecated: 9999)
@available(tvOS, deprecated: 9999)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicStoreInt(
  object target: UnsafeMutablePointer<Int>,
  desired: Int
) {
  UnsafeMutableRawPointer(target)._atomicStoreWord(UInt(bitPattern: desired))
}

// Warning: no overflow checking.
// FIXME: ideally these should not be here, at the very least not public, but
// @usableFromInline internal to be used by SwiftPrivate._stdlib_AtomicInt

@available(*, deprecated)
@available(macOS, deprecated: 9999)
@available(iOS, deprecated: 9999)
@available(watchOS, deprecated: 9999)
@available(tvOS, deprecated: 9999)
public // Existing uses outside stdlib
func _swift_stdlib_atomicFetchAddInt(
  object target: UnsafeMutablePointer<Int>,
  operand: Int
) -> Int {
  let raw = UnsafeMutableRawPointer(target)
  let word = raw._atomicFetchThenWrappingAddWord(UInt(bitPattern: operand))
  return Int(bitPattern: word)
}

@available(*, deprecated)
@available(macOS, deprecated: 9999)
@available(iOS, deprecated: 9999)
@available(watchOS, deprecated: 9999)
@available(tvOS, deprecated: 9999)
public // Existing uses outside stdlib
func _swift_stdlib_atomicFetchAndInt(
  object target: UnsafeMutablePointer<Int>,
  operand: Int
) -> Int {
  let raw = UnsafeMutableRawPointer(target)
  return Int(bitPattern: raw._atomicFetchThenAndWord(UInt(bitPattern: operand)))
}

@available(*, deprecated)
@available(macOS, deprecated: 9999)
@available(iOS, deprecated: 9999)
@available(watchOS, deprecated: 9999)
@available(tvOS, deprecated: 9999)
public // Existing uses outside stdlib
func _swift_stdlib_atomicFetchOrInt(
  object target: UnsafeMutablePointer<Int>,
  operand: Int
) -> Int {
  let raw = UnsafeMutableRawPointer(target)
  return Int(bitPattern: raw._atomicFetchThenOrWord(UInt(bitPattern: operand)))
}

@available(*, deprecated)
@available(macOS, deprecated: 9999)
@available(iOS, deprecated: 9999)
@available(watchOS, deprecated: 9999)
@available(tvOS, deprecated: 9999)
public // Existing uses outside stdlib
func _swift_stdlib_atomicFetchXorInt(
  object target: UnsafeMutablePointer<Int>,
  operand: Int
) -> Int {
  let raw = UnsafeMutableRawPointer(target)
  return Int(bitPattern: raw._atomicFetchThenXorWord(UInt(bitPattern: operand)))
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchAddInt32(
  object target: UnsafeMutablePointer<Int32>,
  operand: Int32
) -> Int32 {
  let value = Builtin.atomicrmw_add_seqcst_Int32(
    target._rawValue,
    operand._value)
  return Int32(value)
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchAddInt64(
  object target: UnsafeMutablePointer<Int64>,
  operand: Int64
) -> Int64 {
  let value = Builtin.atomicrmw_add_seqcst_Int64(
    target._rawValue, operand._value)
  return Int64(value)
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchAndInt32(
  object target: UnsafeMutablePointer<Int32>,
  operand: Int32
) -> Int32 {
  let value = Builtin.atomicrmw_and_seqcst_Int32(
    target._rawValue, operand._value)
  return Int32(value)
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchAndInt64(
  object target: UnsafeMutablePointer<Int64>,
  operand: Int64
) -> Int64 {
  let value = Builtin.atomicrmw_and_seqcst_Int64(
    target._rawValue, operand._value)
  return Int64(value)
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchOrInt32(
  object target: UnsafeMutablePointer<Int32>,
  operand: Int32
) -> Int32 {
  let value = Builtin.atomicrmw_or_seqcst_Int32(
    target._rawValue, operand._value)
  return Int32(value)
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchOrInt64(
  object target: UnsafeMutablePointer<Int64>,
  operand: Int64
) -> Int64 {
  let value = Builtin.atomicrmw_or_seqcst_Int64(
    target._rawValue, operand._value)
  return Int64(value)
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchXorInt32(
  object target: UnsafeMutablePointer<Int32>,
  operand: Int32
) -> Int32 {
  let value = Builtin.atomicrmw_xor_seqcst_Int32(
    target._rawValue, operand._value)
  return Int32(value)
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchXorInt64(
  object target: UnsafeMutablePointer<Int64>,
  operand: Int64
) -> Int64 {
  let value = Builtin.atomicrmw_xor_seqcst_Int64(
    target._rawValue, operand._value)
  return Int64(value)
}
