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
    _valuePtr._atomicStoreWord(desired)
  }

  public func load() -> Int {
    return _valuePtr._atomicLoadWord()
  }

  @discardableResult
  public func fetchAndAdd(_ operand: Int) -> Int {
    return _valuePtr._atomicFetchThenAddWord(operand)
  }
  @discardableResult
  public func fetchAndAnd(_ operand: Int) -> Int {
    return _valuePtr._atomicFetchThenAndWord(operand)
  }
  @discardableResult
  public func fetchAndOr(_ operand: Int) -> Int {
    return _valuePtr._atomicFetchThenOrWord(operand)
  }
  @discardableResult
  public func fetchAndXor(_ operand: Int) -> Int {
    return _valuePtr._atomicFetchThenXorWord(operand)
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
    return _valuePtr._atomicCompareExchangeWord(
      expected: &expected,
      desired: desired)
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
  return UnsafeMutableRawPointer(target)._atomicCompareExchangeWord(
    expected: &expected.pointee,
    desired: desired)
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
  return UnsafeMutableRawPointer(target)._atomicLoadWord()
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
  UnsafeMutableRawPointer(target)._atomicStoreWord(desired)
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
  return UnsafeMutableRawPointer(target)._atomicFetchThenAddWord(operand)
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
  return UnsafeMutableRawPointer(target)._atomicFetchThenAndWord(operand)
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
  return UnsafeMutableRawPointer(target)._atomicFetchThenOrWord(operand)
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
  return UnsafeMutableRawPointer(target)._atomicFetchThenXorWord(operand)
}

// Warning: no overflow checking.
@available(*, deprecated)
@usableFromInline // used by SwiftPrivate._stdlib_AtomicInt
internal func _swift_stdlib_atomicFetchAddInt32(
  object target: UnsafeMutablePointer<Int32>,
  operand: Int32
) -> Int32 {
  let value = Builtin.atomicrmw_add_seqcst_Int32(
    target._rawValue, operand._value)
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
