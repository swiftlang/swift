//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// Automatic Reference Counting (ARC) support for Swift
///
/// This module provides Swift-level interfaces for memory management,
/// weak references, unowned references, and cycle detection.

//===----------------------------------------------------------------------===//
// MARK: - Reference Counting Protocols
//===----------------------------------------------------------------------===//

/// A protocol for types that participate in automatic reference counting.
public protocol AnyObject: class {
  // Marker protocol - all classes automatically conform
}

/// A protocol for objects that can be weakly referenced.
public protocol Weakable: AnyObject {
  // Marker protocol for weak reference support
}

//===----------------------------------------------------------------------===//
// MARK: - Weak Reference Support
//===----------------------------------------------------------------------===//

/// A weak reference to an object.
///
/// Weak references allow you to refer to an object without keeping it alive.
/// When the object is deallocated, weak references automatically become nil.
///
/// Use weak references to break reference cycles:
///
///     class Parent {
///         var children: [Child] = []
///     }
///
///     class Child {
///         weak var parent: Parent?
///     }
///
@frozen
public struct Weak<T: AnyObject> {
  @usableFromInline
  internal var _object: T?
  
  /// Creates a weak reference to the given object.
  @inlinable
  public init(_ object: T?) {
    self._object = object
    if let obj = object {
      _weakRetain(obj)
    }
  }
  
  /// Creates a nil weak reference.
  @inlinable
  public init() {
    self._object = nil
  }
  
  deinit {
    if let obj = _object {
      _weakRelease(obj)
    }
  }
  
  /// The object referenced by this weak reference, or nil if it has been deallocated.
  @inlinable
  public var object: T? {
    get {
      guard let obj = _object else { return nil }
      
      // Try to retain the object to ensure it's still alive
      if _tryRetain(obj) {
        defer { _release(obj) }
        return obj
      }
      
      // Object has been deallocated
      _object = nil
      return nil
    }
    set {
      if let oldObj = _object {
        _weakRelease(oldObj)
      }
      
      _object = newValue
      
      if let newObj = newValue {
        _weakRetain(newObj)
      }
    }
  }
}

extension Weak: Equatable where T: Equatable {
  public static func == (lhs: Weak<T>, rhs: Weak<T>) -> Bool {
    return lhs.object == rhs.object
  }
}

extension Weak: Hashable where T: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(object)
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Unowned Reference Support
//===----------------------------------------------------------------------===//

/// An unowned reference to an object.
///
/// Unowned references allow you to refer to an object without keeping it alive,
/// but unlike weak references, they assume the object will outlive the reference.
/// Accessing a deallocated unowned reference is a runtime error.
///
/// Use unowned references when you know the referenced object will outlive
/// the reference:
///
///     class Customer {
///         var card: CreditCard?
///     }
///
///     class CreditCard {
///         unowned let customer: Customer
///         
///         init(customer: Customer) {
///             self.customer = customer
///         }
///     }
///
@frozen
public struct Unowned<T: AnyObject> {
  @usableFromInline
  internal var _object: T
  
  /// Creates an unowned reference to the given object.
  @inlinable
  public init(_ object: T) {
    self._object = object
  }
  
  /// The object referenced by this unowned reference.
  ///
  /// Accessing this property when the referenced object has been deallocated
  /// results in a runtime error.
  @inlinable
  public var object: T {
    get {
      // Check if object is being deallocated
      if _isBeingDeallocated(_object) {
        _fatalError("Attempted to access deallocated unowned reference")
      }
      return _object
    }
  }
}

extension Unowned: Equatable where T: Equatable {
  public static func == (lhs: Unowned<T>, rhs: Unowned<T>) -> Bool {
    return lhs.object == rhs.object
  }
}

extension Unowned: Hashable where T: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(object)
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Memory Management Utilities
//===----------------------------------------------------------------------===//

/// Returns the current reference count of an object.
///
/// This function is primarily useful for debugging and testing.
/// Production code should not rely on specific reference count values.
///
/// - Parameter object: The object to query.
/// - Returns: The current strong reference count.
@inlinable
public func referenceCount<T: AnyObject>(of object: T) -> Int {
  return _getReferenceCount(object)
}

/// Returns the current weak reference count of an object.
///
/// This function is primarily useful for debugging and testing.
///
/// - Parameter object: The object to query.
/// - Returns: The current weak reference count.
@inlinable
public func weakReferenceCount<T: AnyObject>(of object: T) -> Int {
  return _getWeakReferenceCount(object)
}

/// Checks if two object references point to the same instance.
///
/// This function compares object identity, not value equality.
///
/// - Parameters:
///   - lhs: The first object reference.
///   - rhs: The second object reference.
/// - Returns: `true` if both references point to the same object instance.
@inlinable
public func === <T: AnyObject>(lhs: T?, rhs: T?) -> Bool {
  switch (lhs, rhs) {
  case (nil, nil):
    return true
  case (let l?, let r?):
    return _getObjectIdentifier(l) == _getObjectIdentifier(r)
  default:
    return false
  }
}

/// Checks if two object references point to different instances.
///
/// This function compares object identity, not value equality.
///
/// - Parameters:
///   - lhs: The first object reference.
///   - rhs: The second object reference.
/// - Returns: `true` if the references point to different object instances.
@inlinable
public func !== <T: AnyObject>(lhs: T?, rhs: T?) -> Bool {
  return !(lhs === rhs)
}

//===----------------------------------------------------------------------===//
// MARK: - Memory Management Debugging
//===----------------------------------------------------------------------===//

/// Memory management debugging utilities.
public enum ARCDebug {
  /// Enable or disable ARC debugging.
  ///
  /// When enabled, the runtime will log retain/release operations
  /// and provide additional debugging information.
  ///
  /// - Parameter enabled: Whether to enable debugging.
  public static func setDebugging(_ enabled: Bool) {
    _setARCDebugging(enabled)
  }
  
  /// Check for potential reference cycles.
  ///
  /// This function analyzes all live objects and reports potential
  /// reference cycles. This is a debugging feature and should not
  /// be used in production code.
  public static func checkForCycles() {
    _checkForReferenceCycles()
  }
  
  /// Get current ARC statistics.
  ///
  /// Returns information about allocations, deallocations, and
  /// reference counting operations.
  ///
  /// - Returns: Current ARC runtime statistics.
  public static func getStatistics() -> ARCStatistics {
    return _getARCStatistics()
  }
}

/// ARC runtime statistics for debugging and profiling.
public struct ARCStatistics {
  /// Total number of object allocations.
  public let totalAllocations: UInt64
  
  /// Total number of object deallocations.
  public let totalDeallocations: UInt64
  
  /// Current number of live objects.
  public let currentAllocations: UInt64
  
  /// Peak number of simultaneous live objects.
  public let peakAllocations: UInt64
  
  /// Total number of retain operations.
  public let totalRetains: UInt64
  
  /// Total number of release operations.
  public let totalReleases: UInt64
  
  /// Total number of weak retain operations.
  public let totalWeakRetains: UInt64
  
  /// Total number of weak release operations.
  public let totalWeakReleases: UInt64
  
  /// Initialize statistics structure.
  public init(totalAllocations: UInt64, totalDeallocations: UInt64,
              currentAllocations: UInt64, peakAllocations: UInt64,
              totalRetains: UInt64, totalReleases: UInt64,
              totalWeakRetains: UInt64, totalWeakReleases: UInt64) {
    self.totalAllocations = totalAllocations
    self.totalDeallocations = totalDeallocations
    self.currentAllocations = currentAllocations
    self.peakAllocations = peakAllocations
    self.totalRetains = totalRetains
    self.totalReleases = totalReleases
    self.totalWeakRetains = totalWeakRetains
    self.totalWeakReleases = totalWeakReleases
  }
}

extension ARCStatistics: CustomStringConvertible {
  public var description: String {
    return """
    ARC Statistics:
      Total allocations: \(totalAllocations)
      Total deallocations: \(totalDeallocations)
      Current allocations: \(currentAllocations)
      Peak allocations: \(peakAllocations)
      Total retains: \(totalRetains)
      Total releases: \(totalReleases)
      Total weak retains: \(totalWeakRetains)
      Total weak releases: \(totalWeakReleases)
    """
  }
}

//===----------------------------------------------------------------------===//
// MARK: - Runtime Function Declarations
//===----------------------------------------------------------------------===//

// These functions are implemented in the runtime and called by the compiler

@_silgen_name("swift_retain")
internal func _retain<T: AnyObject>(_ object: T)

@_silgen_name("swift_release")
internal func _release<T: AnyObject>(_ object: T)

@_silgen_name("swift_weakRetain")
internal func _weakRetain<T: AnyObject>(_ object: T)

@_silgen_name("swift_weakRelease")
internal func _weakRelease<T: AnyObject>(_ object: T)

@_silgen_name("swift_tryRetain")
internal func _tryRetain<T: AnyObject>(_ object: T) -> Bool

@_silgen_name("swift_isBeingDeallocated")
internal func _isBeingDeallocated<T: AnyObject>(_ object: T) -> Bool

@_silgen_name("swift_getReferenceCount")
internal func _getReferenceCount<T: AnyObject>(_ object: T) -> Int

@_silgen_name("swift_getWeakReferenceCount")
internal func _getWeakReferenceCount<T: AnyObject>(_ object: T) -> Int

@_silgen_name("swift_getObjectIdentifier")
internal func _getObjectIdentifier<T: AnyObject>(_ object: T) -> UInt

@_silgen_name("swift_setARCDebugging")
internal func _setARCDebugging(_ enabled: Bool)

@_silgen_name("swift_checkForReferenceCycles")
internal func _checkForReferenceCycles()

@_silgen_name("swift_getARCStatistics")
internal func _getARCStatistics() -> ARCStatistics

@_silgen_name("swift_fatalError")
internal func _fatalError(_ message: String) -> Never

//===----------------------------------------------------------------------===//
// MARK: - Automatic Reference Counting
//===----------------------------------------------------------------------===//

/// Manually retain an object.
///
/// This function is primarily for advanced use cases and debugging.
/// Normal Swift code should rely on automatic reference counting.
///
/// - Parameter object: The object to retain.
@inlinable
public func retain<T: AnyObject>(_ object: T) {
  _retain(object)
}

/// Manually release an object.
///
/// This function is primarily for advanced use cases and debugging.
/// Normal Swift code should rely on automatic reference counting.
///
/// - Parameter object: The object to release.
@inlinable
public func release<T: AnyObject>(_ object: T) {
  _release(object)
}

/// Create a weak reference to an object.
///
/// This is a convenience function for creating weak references.
///
/// - Parameter object: The object to weakly reference.
/// - Returns: A weak reference to the object.
@inlinable
public func weak<T: AnyObject>(_ object: T?) -> Weak<T> {
  return Weak(object)
}

/// Create an unowned reference to an object.
///
/// This is a convenience function for creating unowned references.
///
/// - Parameter object: The object to reference unowned.
/// - Returns: An unowned reference to the object.
@inlinable
public func unowned<T: AnyObject>(_ object: T) -> Unowned<T> {
  return Unowned(object)
}

//===----------------------------------------------------------------------===//
// MARK: - withExtendedLifetime
//===----------------------------------------------------------------------===//

/// Evaluates a closure while ensuring that the given instance is not
/// destroyed before the closure returns.
///
/// This function is useful when you need to guarantee that an object
/// remains alive for the duration of a particular operation.
///
/// - Parameters:
///   - x: An instance to preserve until the closure returns.
///   - body: A closure to evaluate.
/// - Returns: The result of the closure.
@inlinable
public func withExtendedLifetime<T, Result>(
  _ x: T, _ body: () throws -> Result
) rethrows -> Result {
  defer { _fixLifetime(x) }
  return try body()
}

/// Evaluates a closure while ensuring that the given instance is not
/// destroyed before the closure returns.
///
/// - Parameters:
///   - x: An instance to preserve until the closure returns.
///   - body: A closure that takes the instance as a parameter.
/// - Returns: The result of the closure.
@inlinable
public func withExtendedLifetime<T, Result>(
  _ x: T, _ body: (T) throws -> Result
) rethrows -> Result {
  defer { _fixLifetime(x) }
  return try body(x)
}

@_silgen_name("swift_fixLifetime")
internal func _fixLifetime<T>(_ x: T)