
/// Returns the dsohandle for the dynamic library.
public func libraryHandle() -> UnsafeRawPointer {
  return #dsohandle
}

/// Prepends "client:" or "library:" to the given string (depending on whether
/// the dsohandle belongs to the library or not) and then prints it.
public func testPrint(handle: UnsafeRawPointer, _ string: String) {
  let libraryHandle = #dsohandle
  let prefix = (handle == libraryHandle) ? "library" : "client"
  print("\(prefix): \(string)")
}

/// Returns true if the host OS version is BackDeploy 2.0 or later, indicating
/// that APIs back deployed before 2.0 should run in the library.
public func isV2OrLater() -> Bool {
  if #available(BackDeploy 2.0, *) {
    return true
  } else {
    return false
  }
}

/// Returns true if BackDeploy 2.0 APIs have been stripped from the binary.
public func v2APIsAreStripped() -> Bool {
#if STRIP_V2_APIS
  return true
#else
  return false
#endif // STRIP_V2_APIS
}

public enum BadError: Error, Equatable {
  /// Indicates badness
  case bad
}

/// A totally unnecessary wrapper for `Int` that adds mutability.
public struct MutableInt {
  @usableFromInline
  internal var _value: Int

  public init(_ value: Int) { _value = value }
}

/// A totally unnecessary wrapper for `Int` that provides reference semantics.
public class ReferenceInt {
  @usableFromInline
  internal var _value: Int

  public init(_ value: Int) { _value = value }
}

/// Describes types that can be incremented.
public protocol Incrementable {
  associatedtype Operand

  mutating func incrementByOne() -> String
  mutating func increment(by amount: Operand) -> Operand
}

extension MutableInt: Incrementable {
  public mutating func incrementByOne() -> String {
    _value += 1
    return String(_value)
  }

  public mutating func increment(by amount: Int) -> Int {
    _value += amount
    return _value
  }
}

extension ReferenceInt: Incrementable {
  public func incrementByOne() -> String {
    _value += 1
    return String(_value)
  }

  public func increment(by amount: Int) -> Int {
    _value += amount
    return _value
  }
}

extension Int {
  @usableFromInline internal func byte(at index: Int) -> UInt8 {
    UInt8(truncatingIfNeeded: self >> (index * 8))
  }
}

// MARK: - Back deployed APIs

#if !STRIP_V2_APIS

@available(BackDeploy 1.0, *)
@_backDeploy(BackDeploy 2.0)
public func trivial() {
  testPrint(handle: #dsohandle, "trivial")
}

@available(BackDeploy 1.0, *)
@_backDeploy(BackDeploy 2.0)
public func pleaseThrow(_ shouldThrow: Bool) throws -> Bool {
  if shouldThrow { throw BadError.bad }
  return !shouldThrow
}

@available(BackDeploy 1.0, *)
@_backDeploy(BackDeploy 2.0)
public func genericIncrement<T: Incrementable>(
  _ x: inout T,
  by amount: T.Operand
) -> T.Operand {
  return x.increment(by: amount)
}

@available(BackDeploy 1.0, *)
@_backDeploy(BackDeploy 2.0)
public func existentialIncrementByOne(_ x: inout any Incrementable) {
  testPrint(handle: #dsohandle, x.incrementByOne())
}

extension MutableInt {
  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public var value: Int { _value }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public func print() {
    // Tests recursive @_backDeploy since `value` is also @_backDeploy
    testPrint(handle: #dsohandle, String(value))
  }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public static var zero: Self { MutableInt(0) }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public mutating func decrement(by amount: Int) -> Int {
    _value -= amount
    return _value
  }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public func toIncrementable() -> any Incrementable { self }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public subscript(byteAt index: Int) -> UInt8 { _value.byte(at: index) }
}

extension ReferenceInt {
  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public final var value: Int { _value }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public final func print() {
    // Tests recursive use of back deployed APIs, since `value` is also
    testPrint(handle: #dsohandle, String(value))
  }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public final func copy() -> ReferenceInt {
    return ReferenceInt(value)
  }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public final class var zero: ReferenceInt { ReferenceInt(0) }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public final func decrement(by amount: Int) -> Int {
    _value -= amount
    return _value
  }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public final func toIncrementable() -> any Incrementable { self }

  @available(BackDeploy 1.0, *)
  @_backDeploy(BackDeploy 2.0)
  public final subscript(byteAt index: Int) -> UInt8 { _value.byte(at: index) }
}

#endif // !STRIP_V2_APIS
