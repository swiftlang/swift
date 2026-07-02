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

/// A tear-off interface implementation that is created on first
/// `QueryInterface` and cached for the lifetime of the owning object.
///
/// `CachedTearOff` is used with ``COMInterfaceResolver`` to lazily provide a
/// COM interface that is expensive to set up but stateless once created. The
/// first call to ``resolve(_:)`` matching the interface's IID creates the
/// implementation; subsequent calls return the cached instance.
///
/// The tear-off delegates its `IUnknown` to the owning object (the
/// controlling-unknown pattern), satisfying the COM identity rule:
/// `QueryInterface` for `IUnknown` on the tear-off returns the owner's primary
/// interface pointer.
///
/// ```swift
/// @com(implementation: "...")
/// final class CImplementation: IInterface, COMInterfaceResolver {
///     private var accessibility = CachedTearOff<IAccessible> {
///         AccessibilityImpl(owner: self)
///     }
///
///     func resolve(_ iid: borrowing IID) -> UnsafeMutableRawPointer? {
///         accessibility.resolve(iid)
///     }
/// }
/// ```
public struct CachedTearOff<Interface: IUnknown> {
  private let factory: () -> Interface
  private lazy var instance: Interface = factory()

  /// Creates a cached tear-off that will use `factory` to create the
  /// implementation on first query.
  ///
  /// The factory is called at most once. The returned object must delegate its
  /// `IUnknown` to the owning object's controlling unknown.
  ///
  /// - Parameter factory: A closure that creates the tear-off implementation.
  public init(_ factory: @escaping () -> Interface) {
    self.factory = factory
  }

  /// Returns the COM interface pointer if `iid` matches `T.IID`, or `nil`
  /// otherwise.
  ///
  /// On the first matching call, the factory is invoked and the result is
  /// cached. Subsequent calls return the cached instance.
  ///
  /// - Parameter iid: The interface identifier being queried.
  /// - Returns: A COM interface pointer, or `nil` if `iid` does not match
  ///   `T.IID`.
  public mutating func resolve(_ riid: borrowing IID) -> UnsafeMutableRawPointer? {
    guard riid == Interface.IID else { return nil }
    return UnsafeMutableRawPointer(unsafeCOMPointer: instance)
  }
}

/// A tear-off interface implementation that creates a fresh instance on every
/// `QueryInterface`.
///
/// `DisposableTearOff` is used with ``COMInterfaceResolver`` to provide a COM
/// interface where each caller receives its own independently reference-counted
/// instance. This is suited to interfaces that hold per-caller state (e.g.,
/// enumerators, connection points).
///
/// Each instance returned by ``resolve(_:)`` delegates its `IUnknown` to the
/// owning object (the controlling-unknown pattern), satisfying the COM identity
/// rule.
///
/// ```swift
/// @com(implementation: "...")
/// final class CImplementation: IInterface, COMInterfaceResolver {
///     private let connectionPoint = DisposableTearOff<IConnectionPoint> {
///         ConnectionPointImpl()
///     }
///
///     func resolve(_ iid: borrowing IID) -> UnsafeMutableRawPointer? {
///         connectionPoint.resolve(iid)
///     }
/// }
/// ```
public struct DisposableTearOff<Interface: IUnknown> {
  private let factory: () -> Interface

  /// Creates a disposable tear-off that will use `factory` to create a fresh
  /// implementation on every query.
  ///
  /// Each invocation of the factory must return a new instance. The returned
  /// object must delegate its `IUnknown` to the owning object's controlling
  /// unknown.
  ///
  /// - Parameter factory: A closure that creates a tear-off implementation.
  public init(_ factory: @escaping () -> Interface) {
    self.factory = factory
  }

  /// Returns a fresh COM interface pointer if `iid` matches `T.IID`, or `nil`
  /// otherwise.
  ///
  /// Each call creates a new instance via the factory. The caller receives an
  /// independently reference-counted object.
  ///
  /// - Parameter iid: The interface identifier being queried.
  /// - Returns: A COM interface pointer to a new instance, or `nil` if `iid`
  ///   does not match `T.IID`.
  public func resolve(_ riid: borrowing IID) -> UnsafeMutableRawPointer? {
    guard riid == Interface.IID else { return nil }
    return UnsafeMutableRawPointer(unsafeCOMPointer: factory())
  }
}
