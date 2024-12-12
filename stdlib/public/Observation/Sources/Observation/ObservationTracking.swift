//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
public struct ObservationTracking: Sendable {
  enum Id {
    case willSet(Int)
    case didSet(Int)
    case full(Int, Int)
  }

  struct Entry: @unchecked Sendable {
    let context: ObservationRegistrar.Context
    
    var properties: Set<AnyKeyPath>
    
    init(_ context: ObservationRegistrar.Context, properties: Set<AnyKeyPath> = []) {
      self.context = context
      self.properties = properties
    }
    
    func addWillSetObserver(_ changed: @Sendable @escaping (AnyKeyPath) -> Void) -> Int {
      return context.registerTracking(for: properties, willSet: changed)
    }

    func addDidSetObserver(_ changed: @Sendable @escaping (AnyKeyPath) -> Void) -> Int {
      return context.registerTracking(for: properties, didSet: changed)
    }
    
    func removeObserver(_ token: Int) {
      context.cancel(token)
    }
    
    mutating func insert(_ keyPath: AnyKeyPath) {
      properties.insert(keyPath)
    }
    
    func union(_ entry: Entry) -> Entry {
      Entry(context, properties: properties.union(entry.properties))
    }
  }
  
  @_spi(SwiftUI)
  public struct _AccessList: Sendable {
    internal var entries = [ObjectIdentifier : Entry]()

    internal init() { }
    
    internal mutating func addAccess<Subject: Observable>(
      keyPath: PartialKeyPath<Subject>,
      context: ObservationRegistrar.Context
    ) {
      entries[context.id, default: Entry(context)].insert(keyPath)
    }
    
    internal mutating func merge(_ other: _AccessList) {
      entries.merge(other.entries) { existing, entry in
        existing.union(entry)
      }
    }
    
    func contains(_ keyPath: AnyKeyPath) -> Bool {
      for entry in entries.values {
        if entry.properties.contains(keyPath) { return true }
      }
      return false
    }
  }

  @_spi(SwiftUI)
  public static func _installTracking(
    _ tracking: ObservationTracking,
    willSet: (@Sendable (ObservationTracking) -> Void)? = nil,
    didSet: (@Sendable (ObservationTracking) -> Void)? = nil
  ) {
    let values = tracking.list.entries.mapValues {
      switch (willSet, didSet) {
      case (.some(let willSetObserver), .some(let didSetObserver)):
        return Id.full($0.addWillSetObserver { keyPath in
          tracking.state.withCriticalRegion { $0.changed = keyPath }
          willSetObserver(tracking)
        }, $0.addDidSetObserver { keyPath in
          tracking.state.withCriticalRegion { $0.changed = keyPath }
          didSetObserver(tracking)
        })
      case (.some(let willSetObserver), .none):
        return Id.willSet($0.addWillSetObserver { keyPath in
          tracking.state.withCriticalRegion { $0.changed = keyPath }
          willSetObserver(tracking)
        })
      case (.none, .some(let didSetObserver)):
        return Id.didSet($0.addDidSetObserver { keyPath in
          tracking.state.withCriticalRegion { $0.changed = keyPath }
          didSetObserver(tracking)
        })
      case (.none, .none):
        fatalError()
      }
    }
    
    tracking.install(values)
  }

  @_spi(SwiftUI)
  public static func _installTracking(
    _ list: _AccessList,
    onChange: @escaping @Sendable () -> Void
  ) {
    let tracking = ObservationTracking(list)
    _installTracking(tracking, willSet: { _ in
      onChange()
      tracking.cancel()
    })
  }

  struct State {
    var values = [ObjectIdentifier: ObservationTracking.Id]()
    var cancelled = false
    var changed: AnyKeyPath?
  }
  
  private let state = _ManagedCriticalState(State())
  private let list: _AccessList
  
  @_spi(SwiftUI)
  public init(_ list: _AccessList?) {
    self.list = list ?? _AccessList()
  }

  internal func install(_ values:  [ObjectIdentifier : ObservationTracking.Id]) {
    state.withCriticalRegion {
      if !$0.cancelled {
        $0.values = values
      }
    }
  }
  
  /// A representation of a tracked key path that avoids the potential of
  /// allowing un-privledged access to subscripting of objects
  @available(SwiftStdlib 9999, *)
  public struct Path {
    var keyPath: AnyKeyPath
    public var rootType: Any.Type {
      type(of: keyPath).rootType
    }
    
    public var valueType: Any.Type {
      type(of: keyPath).valueType
    }
    
    public static func ==(_ lhs: Path, _ rhs: AnyKeyPath) -> Bool {
      lhs.keyPath == rhs
    }
    
    public static func !=(_ lhs: Path, _ rhs: AnyKeyPath) -> Bool {
      lhs.keyPath != rhs
    }
    
    public static func ==(_ lhs: AnyKeyPath, _ rhs: Path) -> Bool {
      lhs == rhs.keyPath
    }
    
    public static func !=(_ lhs: AnyKeyPath, _ rhs: Path) -> Bool {
      lhs != rhs.keyPath
    }
  }
  
  @available(SwiftStdlib 9999, *)
  public struct Options: ExpressibleByArrayLiteral {
    enum Value {
      case exclusive
      case cancelOnFirstChange
      case filter(@Sendable (Path) -> Bool)
    }
    
    var expanded: (exclusive: Bool, cancelOnFirstChange: Bool, filters: [@Sendable (Path) -> Bool]) {
      var result: (exclusive: Bool, cancelOnFirstChange: Bool, filters: [@Sendable (Path) -> Bool]) = (false, false, [])
      for value in values {
        switch value {
        case .exclusive:
          result.exclusive = true
        case .cancelOnFirstChange:
          result.cancelOnFirstChange = true
        case .filter(let filter):
          result.filters.append(filter)
        }
      }
      return result
    }
    
    var values: [Value]
    
    init(_ value: Value) {
      self.values = [value]
    }
    
    public init(arrayLiteral elements: Options...) {
      values = elements.flatMap { $0.values }
    }
    /// Nested observation will exclude this tracking set from change
    /// notification to its parent
    public static var exclusive: Options {
      Options(.exclusive)
    }
    /// This tracking will cancel upon the first change observed
    public static var cancelOnFirstChange: Options {
      Options(.cancelOnFirstChange)
    }
    /// Filter the accesses in application of `withObservationTracking` by the
    /// result of the `predicate` parameter. 
    public static func filtering(
      _ predicate: @Sendable @escaping (Path) -> Bool
    ) -> Options {
      Options(.filter(predicate))
    }
  }

  @available(SwiftStdlib 9999, *)
  /// Verification that a given property is tracked
  public func contains<Object, Member>(
    _ keyPath: KeyPath<Object, Member>
  ) -> Bool {
    list.contains(keyPath)
  }
  
  @available(SwiftStdlib 9999, *)
  public var changedPath: Path? {
    changed.map { Path(keyPath: $0) }
  }

  public func cancel() {
    let values = state.withCriticalRegion {
      $0.cancelled = true
      let values = $0.values
      $0.values = [:]
      return values
    }
    for (id, observationId) in values {
        switch observationId {
        case .willSet(let token):
          list.entries[id]?.removeObserver(token)
        case .didSet(let token):
          list.entries[id]?.removeObserver(token)
        case .full(let willSetToken, let didSetToken):
          list.entries[id]?.removeObserver(willSetToken)
          list.entries[id]?.removeObserver(didSetToken)
        }
      }
  }

  @available(SwiftStdlib 6.0, *)
  public var changed: AnyKeyPath? {
      state.withCriticalRegion { $0.changed }
  }
}

@available(SwiftStdlib 5.9, *)
fileprivate func generateAccessList<T>(exclusive: Bool = false, _ apply: () -> T) -> (T, ObservationTracking._AccessList?) {
  var accessList: ObservationTracking._AccessList?
  let result = withUnsafeMutablePointer(to: &accessList) { ptr -> T in
    let previous = _ThreadLocal.value
    _ThreadLocal.value = UnsafeMutableRawPointer(ptr)
    defer {
      if let scoped = ptr.pointee, let previous {
        if var prevList = previous.assumingMemoryBound(to: ObservationTracking._AccessList?.self).pointee {
          if !exclusive {
            prevList.merge(scoped)
          }
          previous.assumingMemoryBound(to: ObservationTracking._AccessList?.self).pointee = prevList
        } else {
          previous.assumingMemoryBound(to: ObservationTracking._AccessList?.self).pointee = scoped
        }
      }
      _ThreadLocal.value = previous
    }
    return apply()
  }
  return (result, accessList)
}

/// Tracks access to properties.
///
/// This method tracks access to any property within the `apply` closure, and
/// informs the caller of value changes made to participating properties by way
/// of the `onChange` closure. For example, the following code tracks changes
/// to the name of cars, but it doesn't track changes to any other property of
/// `Car`:
///
///     func render() {
///         withObservationTracking {
///             for car in cars {
///                 print(car.name)
///             }
///         } onChange: {
///             print("Schedule renderer.")
///         }
///     }
///
/// - Parameters:
///     - apply: A closure that contains properties to track.
///     - onChange: The closure invoked when the value of a property changes.
///
/// - Returns: The value that the `apply` closure returns if it has a return
/// value; otherwise, there is no return value.
@available(SwiftStdlib 5.9, *)
public func withObservationTracking<T>(
  _ apply: () -> T,
  onChange: @autoclosure () -> @Sendable () -> Void
) -> T {
  let (result, accessList) = generateAccessList(apply)
  if let accessList {
    ObservationTracking._installTracking(accessList, onChange: onChange())
  }
  return result
}

@available(SwiftStdlib 5.9, *)
@_disfavoredOverload
@_spi(SwiftUI)
public func withObservationTracking<T>(
  _ apply: () -> T,
  willSet: @escaping @Sendable (ObservationTracking) -> Void,
  didSet: @escaping @Sendable (ObservationTracking) -> Void
) -> T {
  let (result, accessList) = generateAccessList(apply)
  ObservationTracking._installTracking(ObservationTracking(accessList), willSet: willSet, didSet: didSet)
  return result
}

@available(SwiftStdlib 5.9, *)
@_disfavoredOverload
@_spi(SwiftUI)
public func withObservationTracking<T>(
  _ apply: () -> T,
  willSet: @escaping @Sendable (ObservationTracking) -> Void
) -> T {
  let (result, accessList) = generateAccessList(apply)
  ObservationTracking._installTracking(ObservationTracking(accessList), willSet: willSet, didSet: nil)
  return result
}

@available(SwiftStdlib 5.9, *)
@_disfavoredOverload
@_spi(SwiftUI)
public func withObservationTracking<T>(
  _ apply: () -> T,
  didSet: @escaping @Sendable (ObservationTracking) -> Void
) -> T {
  let (result, accessList) = generateAccessList(apply)
  ObservationTracking._installTracking(ObservationTracking(accessList), willSet: nil, didSet: didSet)
  return result
}

@available(SwiftStdlib 9999, *)
fileprivate func _withObservationTracking<T>(
   options: ObservationTracking.Options,
   _ apply: () -> T,
   willSet: (@Sendable (ObservationTracking) -> Void)?,
   didSet: (@Sendable (ObservationTracking) -> Void)?
) -> (T, ObservationTracking) {
  let (exclusive, cancelsOnFirstChange, filters) = options.expanded
  let (result, accessList) = generateAccessList(exclusive: exclusive, apply)
  let tracking = ObservationTracking(accessList)
  switch (cancelsOnFirstChange, filters.isEmpty) {
  case (true, true):
    ObservationTracking._installTracking(tracking, willSet: willSet, didSet: { tracking in
      didSet?(tracking)
      tracking.cancel()
    })
  case (false, true):
    ObservationTracking._installTracking(tracking, willSet: willSet, didSet: didSet)
  case (true, false):
    if let willSet {
      ObservationTracking._installTracking(tracking, willSet: { tracking in
        for filter in filters {
          if !filter(tracking.changedPath!) {
            return
          }
        }
        willSet(tracking)
      }, didSet: { tracking in
        for filter in filters {
          if !filter(tracking.changedPath!) {
            return
          }
        }
        didSet?(tracking)
        tracking.cancel()
      })
    } else {
      ObservationTracking._installTracking(tracking, willSet: nil, didSet: { tracking in
        for filter in filters {
          if !filter(tracking.changedPath!) {
            return
          }
        }
        didSet?(tracking)
        tracking.cancel()
      })
    }
    
  case (false, false):
    if let willSet {
      ObservationTracking._installTracking(tracking, willSet: { tracking in
        for filter in filters {
          if !filter(tracking.changedPath!) {
            return
          }
        }
        willSet(tracking)
      }, didSet: { tracking in
        for filter in filters {
          if !filter(tracking.changedPath!) {
            return
          }
        }
        didSet?(tracking)
      })
    } else {
      ObservationTracking._installTracking(tracking, willSet: nil, didSet: { tracking in
        for filter in filters {
          if !filter(tracking.changedPath!) {
            return
          }
        }
        didSet?(tracking)
      })
    }
  }
  return (result, tracking)
}

@available(SwiftStdlib 9999, *)
public func withObservationTracking<T>(
   options: ObservationTracking.Options = [],
   _ apply: () -> T,
   willSet: @escaping @Sendable (ObservationTracking) -> Void,
   didSet: @escaping @Sendable (ObservationTracking) -> Void
) -> (T, ObservationTracking) {
  _withObservationTracking(options: options, apply, willSet: willSet, didSet: didSet)
}

// Overload to make the willSet optional when the didSet is specified
@available(SwiftStdlib 9999, *)
public func withObservationTracking<T>(
  options: ObservationTracking.Options = [],
  _ apply: () -> T,
  didSet: @escaping @Sendable (ObservationTracking) -> Void
) -> (T, ObservationTracking) {
  _withObservationTracking(options: options, apply, willSet: nil, didSet: didSet)
}

// Overload to make the didSet optional when the willSet is specified
@available(SwiftStdlib 9999, *)
public func withObservationTracking<T>(
  options: ObservationTracking.Options = [],
  _ apply: () -> T,
  willSet: @escaping @Sendable (ObservationTracking) -> Void
) -> (T, ObservationTracking) {
  _withObservationTracking(options: options, apply, willSet: willSet, didSet: nil)
}
