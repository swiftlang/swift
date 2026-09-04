//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
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
/// The tear-off's identity relationship to its owner is determined by the active
/// object model. Models with a controlling identity interface can use their
/// aggregation policy; rootless models can expose an independent object.
///
/// ```swift
/// @com(implementation: "...")
/// final class CImplementation: IInterface, COMInterfaceResolver {
///     private var accessibility = CachedTearOff<IAccessible> {
///         AccessibilityImpl(owner: self)
///     }
///
///     func resolve(_ iid: borrowing IID) -> COMInterfaceResolution? {
///         accessibility.resolve(iid)
///     }
/// }
/// ```
public struct CachedTearOff<Interface>: ~Copyable where Interface.Type: COMInterface {
  private var factory: (() -> Interface)?
  private var instance: Interface?

  /// Creates a cached tear-off that will use `factory` to create the
  /// implementation on first query.
  ///
  /// The factory is called at most once. The returned object must follow the
  /// active object model's identity policy.
  ///
  /// - Parameter factory: A closure that creates the tear-off implementation.
  public init(_ factory: consuming @escaping () -> Interface) {
    self.factory = factory
    self.instance = nil
  }

  /// Returns the COM interface pointer if `iid` matches `Interface.IID`, or
  /// `nil` otherwise.
  ///
  /// On the first matching call, the factory is invoked and the result is
  /// cached. Subsequent calls return the cached instance.
  ///
  /// - Parameter iid: The interface identifier being queried.
  /// - Returns: An owned COM interface result, or `nil` if `iid` does not match
  ///   `Interface.IID`.
  public mutating func resolve(_ riid: borrowing IID) -> COMInterfaceResolution? {
    guard riid == Interface.IID else { return nil }
    if let instance {
      return COMInterfaceResolution(instance)
    }
    guard let factory else {
      preconditionFailure("cached tear-off has no factory or instance")
    }
    let instance = factory()
    self.factory = nil
    self.instance = instance
    return COMInterfaceResolution(instance)
  }
}

private struct AtomicCachedTearOffState<Interface>: ~Copyable {
  var factory: (() -> Interface)?
  var instance: Interface?
}

/// A thread-safe variant of `CachedTearOff` for free-threaded objects.
///
/// `AtomicCachedTearOff` provides the same semantics as `CachedTearOff` —
/// created on first `QueryInterface`, cached for the owner's lifetime — but
/// uses a mutex to handle concurrent `QueryInterface` calls safely. The
/// post-initialization path reads the cached instance while holding that lock.
///
/// Use `AtomicCachedTearOff` whenever the active object model permits concurrent
/// interface discovery. For externally serialized objects, prefer `CachedTearOff`
/// and avoid the synchronization overhead.
///
/// ```swift
/// @com(implementation: "...", threading: .both)
/// final class CImplementation: IInterface, COMInterfaceResolver {
///     private let accessibility = AtomicCachedTearOff<IAccessible> {
///         AccessibilityImpl(owner: self)
///     }
///
///     func resolve(_ iid: borrowing IID) -> COMInterfaceResolution? {
///         accessibility.resolve(iid)
///     }
/// }
/// ```
public struct AtomicCachedTearOff<Interface>: ~Copyable where Interface.Type: COMInterface {
  private let state: Mutex<AtomicCachedTearOffState<Interface>>

  /// Creates an atomic cached tear-off that will use `factory` to create the
  /// implementation on first query.
  ///
  /// The factory is called at most once. The returned object must follow the
  /// active object model's identity policy.
  ///
  /// - Parameter factory: A closure that creates the tear-off implementation.
  public init(_ factory: consuming @escaping () -> Interface) {
    self.state = Mutex(AtomicCachedTearOffState(factory: factory,
                                                instance: nil))
  }

  /// Returns the COM interface pointer if `iid` matches `Interface.IID`, or `nil`
  /// otherwise.
  ///
  /// On the first matching call, the factory is invoked under a lock and the
  /// result is cached. Subsequent calls return the cached instance.
  ///
  /// - Parameter iid: The interface identifier being queried.
  /// - Returns: An owned COM interface result, or `nil` if `iid` does not match
  ///   `Interface.IID`.
  public func resolve(_ riid: borrowing IID) -> COMInterfaceResolution? {
    guard riid == Interface.IID else { return nil }
    return state.withLock { state -> COMInterfaceResolution in
      if let instance = state.instance {
        return COMInterfaceResolution(instance)
      }
      guard let factory = state.factory else {
        preconditionFailure("cached tear-off has no factory or instance")
      }
      let instance = factory()
      state.factory = nil
      state.instance = instance
      return COMInterfaceResolution(instance)
    }
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
///     func resolve(_ iid: borrowing IID) -> COMInterfaceResolution? {
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
  /// object must follow the active object model's identity policy.
  ///
  /// - Parameter factory: A closure that creates a tear-off implementation.
  public init(_ factory: consuming @escaping () -> Interface) {
    self.factory = factory
  }

  /// Returns a fresh COM interface pointer if `iid` matches `Interface.IID`, or
  /// `nil` otherwise.
  ///
  /// Each call creates a new instance via the factory. The caller receives an
  /// independently reference-counted object.
  ///
  /// - Parameter iid: The interface identifier being queried.
  /// - Returns: An owned COM interface result for a new instance, or `nil` if
  ///   `iid` does not match `Interface.IID`.
  public func resolve(_ riid: borrowing IID) -> COMInterfaceResolution? {
    guard riid == Interface.IID else { return nil }
    return COMInterfaceResolution(factory())
  }
}
