import Foundation

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

/// Describes types that can be appended to.
public protocol Appendable {
  associatedtype Element
  mutating func append(_ x: __owned Element)
}

/// Describes types that can be counted.
public protocol Countable {
  var count: Int { get }
}

public enum BadError: Error, Equatable {
  /// Indicates badness
  case bad

  /// Even worse
  case reallyBad
}


/// A totally unnecessary array type for `Int` elements.
public struct IntArray {
  @usableFromInline
  internal var _values: [Int]

  public init(_ values: [Int]) { _values = values }
}

extension IntArray: Appendable {
  public mutating func append(_ x: Int) {
    _values.append(x)
  }
}

extension IntArray: Countable {
  public var count: Int { _values.count }
}


/// A totally unnecessary array type for `Int` elements with reference semantics.
public class ReferenceIntArray {
  @usableFromInline
  internal var _values: [Int]

  public init(_ values: [Int]) { _values = values }
}

extension ReferenceIntArray: Appendable {
  public func append(_ x: Int) {
    _values.append(x)
  }
}

extension ReferenceIntArray: Countable {
  public var count: Int { _values.count }
}

// MARK: - Back deployed APIs

#if !STRIP_V2_APIS

@available(BackDeploy 1.0, *)
@backDeployed(before: BackDeploy 2.0)
public func trivial() {
  testPrint(handle: #dsohandle, "trivial")
}

@available(BackDeploy 1.0, *)
@backDeployed(before: BackDeploy 2.0)
public func pleaseThrow(_ shouldThrow: Bool) throws -> Bool {
  if shouldThrow { throw BadError.bad }
  return !shouldThrow
}

@available(BackDeploy 1.0, *)
@backDeployed(before: BackDeploy 2.0)
public func genericAppend<T: Appendable>(
  _ a: inout T,
  _ x: __owned T.Element
) {
  return a.append(x)
}

@available(BackDeploy 1.0, *)
@backDeployed(before: BackDeploy 2.0)
public func existentialCount(_ c: any Countable) -> Int {
  c.count
}

extension IntArray {
  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public var values: [Int] { _values }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public init() { self.init([]) }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public func print() {
    // Tests recursive @backDeployed since `Array.print()` is also @backDeployed
    _values.print()
  }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public static var empty: Self { IntArray([]) }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public func toCountable() -> any Countable { self }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  @inlinable
  public subscript(_ i: Int) -> Int {
    get { _values[i] }
    _modify { yield &_values[i] }
  }
  
  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public var rawValues: [Int] {
    _read { yield _values }
  }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public mutating func removeLast() -> Int? {
    defer { _values.removeLast() }
    return _values.last
  }
}

extension ReferenceIntArray {
  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public convenience init() { self.init([]) }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public final var values: [Int] { _values }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public final func print() {
    // Tests recursive @backDeployed since `Array.print()` is also @backDeployed
    _values.print()
  }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public final func copy() -> ReferenceIntArray {
    return ReferenceIntArray(values)
  }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public final class var empty: ReferenceIntArray { ReferenceIntArray([]) }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public final func toCountable() -> any Countable { self }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  @inlinable
  public final subscript(_ i: Int) -> Int {
    get { _values[i] }
    _modify { yield &_values[i] }
  }
  
  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public final var rawValues: [Int] {
    _read { yield _values }
  }

  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public final func removeLast() -> Int? {
    defer { _values.removeLast() }
    return _values.last
  }
}

extension Array {
  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public func print() {
    testPrint(handle: #dsohandle, description)
  }
}

extension BadError {
  @available(BackDeploy 1.0, *)
  @backDeployed(before: BackDeploy 2.0)
  public init?(fromEmoji emoji: Character) {
    switch emoji {
    case "❗️": self = .bad
    case "‼️": self = .reallyBad
    default: return nil
    }
  }
}

#endif // !STRIP_V2_APIS
