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

private import Synchronization

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
public struct CachedTearOff<Interface> where Interface.Type: COMInterface {
  private let factory: () -> Interface
  private var instance: Interface = factory()

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

/// A thread-safe variant of `CachedTearOff` for free-threaded objects.
///
/// `AtomicCachedTearOff` provides the same semantics as `CachedTearOff` —
/// created on first `QueryInterface`, cached for the owner's lifetime — but
/// uses atomic initialization to handle concurrent `QueryInterface` calls
/// safely. On the post-initialization fast path, the cost is a single atomic
/// load.
///
/// Use `AtomicCachedTearOff` for `@com` classes with `ThreadingModel: .free`,
/// `.both`, or `.neutral`, where COM permits concurrent calls. For
/// apartment-threaded objects, prefer `CachedTearOff` (no synchronization
/// overhead).
///
/// ```swift
/// @com(implementation: "...", threading: .both)
/// final class CImplementation: IInterface, COMInterfaceResolver {
///     private let accessibility = AtomicCachedTearOff<IAccessible> {
///         AccessibilityImpl(owner: self)
///     }
///
///     func resolve(_ iid: borrowing IID) -> UnsafeMutableRawPointer? {
///         accessibility.resolve(iid)
///     }
/// }
/// ```
public struct AtomicCachedTearOff<Interface>: ~Copyable where Interface.Type: COMInterface {
  private let factory: () -> Interface
  private let state: Mutex<Interface?>

  /// Creates an atomic cached tear-off that will use `factory` to create the
  /// implementation on first query.
  ///
  /// The factory is called at most once. The returned object must delegate its
  /// `IUnknown` to the owning object's controlling unknown via `COMAggregatable`.
  ///
  /// - Parameter factory: A closure that creates the tear-off implementation.
  public init(_ factory: @escaping () -> Interface) {
    self.factory = factory
    self.state = Mutex(nil)
  }

  /// Returns the COM interface pointer if `iid` matches `T.IID`, or `nil`
  /// otherwise.
  ///
  /// On the first matching call, the factory is invoked under a lock and the
  /// result is cached. Subsequent calls return the cached instance.
  ///
  /// - Parameter iid: The interface identifier being queried.
  /// - Returns: A COM interface pointer, or `nil` if `iid` does not match
  ///   `T.IID`.
  public func resolve(_ riid: borrowing IID) -> UnsafeMutableRawPointer? {
    guard riid == Interface.IID else { return nil }
    let instance = state.withLock { cached -> Interface in
      if let cached { return cached }
      let new = factory()
      cached = new
      return new
    }
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
public struct DisposableTearOff<Interface> where Interface.Type: COMInterface {
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
