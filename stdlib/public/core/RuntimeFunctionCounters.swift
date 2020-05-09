//===--- RuntimeFunctionCounters.swift ------------------------------------===//
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
//
//  This file implements the experimental support for collecting the state of
//  runtime function counters, which are used to determine how many times
//  a given runtime function was called.
//
//  It is possible to get the global counters, which represent the total
//  number of invocations, or per-object counters, which represent the
//  number of runtime functions calls for a specific object.

// By default, this feature is enabled only when assertions are enabled. To control it
// separately, set the SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS environment variable when
// invoking build-script:
// SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS=TRUE ./utils/build-script ...
#if SWIFT_ENABLE_RUNTIME_FUNCTION_COUNTERS

/// Collect all references inside the object using Mirrors.
/// - Parameter value: the value to be inspected
/// - Parameter references: the array which should contain the collected
///                         references
/// - Parameter visitedItems: the dictionary for keeping track of visited
///                           objects
internal func _collectAllReferencesInsideObjectImpl(
  _ value: Any,
  references: inout [UnsafeRawPointer],
  visitedItems: inout [ObjectIdentifier: Int]
) {
  // Use the structural reflection and ignore any
  // custom reflectable overrides.
  let mirror = Mirror(internalReflecting: value)

  let id: ObjectIdentifier?
  let ref: UnsafeRawPointer?
  if type(of: value) is AnyObject.Type {
    // Object is a class (but not an ObjC-bridged struct)
    let toAnyObject = _unsafeDowncastToAnyObject(fromAny: value)
    ref = UnsafeRawPointer(Unmanaged.passUnretained(toAnyObject).toOpaque())
    id = ObjectIdentifier(toAnyObject)
  } else if type(of: value) is Builtin.BridgeObject.Type {
    ref = UnsafeRawPointer(
      Builtin.bridgeToRawPointer(value as! Builtin.BridgeObject))
    id = nil
  } else if type(of: value) is Builtin.NativeObject.Type  {
    ref = UnsafeRawPointer(
      Builtin.bridgeToRawPointer(value as! Builtin.NativeObject))
    id = nil
  } else if let metatypeInstance = value as? Any.Type {
    // Object is a metatype
    id = ObjectIdentifier(metatypeInstance)
    ref = nil
  } else {
    id = nil
    ref = nil
  }

  if let theId = id {
    // Bail if this object was seen already.
    if visitedItems[theId] != nil {
      return
    }
    // Remember that this object was seen already.
    let identifier = visitedItems.count
    visitedItems[theId] = identifier
  }

  // If it is a reference, add it to the result.
  if let ref = ref {
    references.append(ref)
  }

  // Recursively visit the children of the current value.
  let count = mirror.children.count
  var currentIndex = mirror.children.startIndex
  for _ in 0..<count {
    let (_, child) = mirror.children[currentIndex]
    mirror.children.formIndex(after: &currentIndex)
    _collectAllReferencesInsideObjectImpl(
      child,
      references: &references,
      visitedItems: &visitedItems)
  }
}

// This is a namespace for runtime functions related to management
// of runtime function counters.
public // @testable
struct _RuntimeFunctionCounters {
#if os(Windows) && arch(x86_64)
  public typealias RuntimeFunctionCountersUpdateHandler =
    @convention(c) (_ object: UnsafeRawPointer, _ functionId: Int) -> Void
#else
  public typealias RuntimeFunctionCountersUpdateHandler =
    @convention(c) (_ object: UnsafeRawPointer, _ functionId: Int64) -> Void
#endif

  public static let runtimeFunctionNames =
    getRuntimeFunctionNames()
  public static let runtimeFunctionCountersOffsets =
    _RuntimeFunctionCounters.getRuntimeFunctionCountersOffsets()
  public static let numRuntimeFunctionCounters =
    Int(_RuntimeFunctionCounters.getNumRuntimeFunctionCounters())
  public static let runtimeFunctionNameToIndex: [String: Int] =
    getRuntimeFunctionNameToIndex()

  /// Get the names of all runtime functions whose calls are being
  /// tracked.
  @_silgen_name("_swift_getRuntimeFunctionNames")
  public static func _getRuntimeFunctionNames() ->
    UnsafePointer<UnsafePointer<CChar>>

  public static func getRuntimeFunctionNames() -> [String] {
    let names = _RuntimeFunctionCounters._getRuntimeFunctionNames()
    let numRuntimeFunctionCounters =
      Int(_RuntimeFunctionCounters.getNumRuntimeFunctionCounters())
    var functionNames: [String] = []
    functionNames.reserveCapacity(numRuntimeFunctionCounters)
    for index in 0..<numRuntimeFunctionCounters {
      let name = String(cString: names[index])
      functionNames.append(name)
    }
    return functionNames
  }

  /// Get the offsets of the collected runtime function counters inside
  /// the state.
  @_silgen_name("_swift_getRuntimeFunctionCountersOffsets")
  public static func getRuntimeFunctionCountersOffsets() ->
    UnsafePointer<UInt16>

  /// Get the number of different runtime functions whose calls are being
  /// tracked.
  @_silgen_name("_swift_getNumRuntimeFunctionCounters")
  public static func getNumRuntimeFunctionCounters() -> UInt64

  /// Dump all per-object runtime function counters.
  @_silgen_name("_swift_dumpObjectsRuntimeFunctionPointers")
  public static func dumpObjectsRuntimeFunctionPointers()

  @discardableResult
  @_silgen_name("_swift_setGlobalRuntimeFunctionCountersUpdateHandler")
  public static func setGlobalRuntimeFunctionCountersUpdateHandler(
    handler: RuntimeFunctionCountersUpdateHandler?
  ) -> RuntimeFunctionCountersUpdateHandler?

  /// Collect all references inside the object using Mirrors.
  public static func collectAllReferencesInsideObject(_ value: Any) ->
    [UnsafeRawPointer] {
    var visited: [ObjectIdentifier: Int] = [:]
    var references: [UnsafeRawPointer] = []
    _collectAllReferencesInsideObjectImpl(
      value, references: &references, visitedItems: &visited)
    return references
  }

  /// Build a map from counter name to counter index inside the state struct.
  internal static func getRuntimeFunctionNameToIndex() -> [String: Int] {
    let runtimeFunctionNames = _RuntimeFunctionCounters.getRuntimeFunctionNames()
    let numRuntimeFunctionCounters =
      Int(_RuntimeFunctionCounters.getNumRuntimeFunctionCounters())
    var runtimeFunctionNameToIndex: [String: Int] = [:]
    runtimeFunctionNameToIndex.reserveCapacity(numRuntimeFunctionCounters)

    for index in 0..<numRuntimeFunctionCounters {
      let name = runtimeFunctionNames[index]
      runtimeFunctionNameToIndex[name] = index
    }
    return runtimeFunctionNameToIndex
  }
}

/// This protocol defines a set of operations for accessing runtime function
/// counters statistics.
public // @testable
protocol _RuntimeFunctionCountersStats: CustomDebugStringConvertible {
  init()

  /// Dump the current state of all counters.
  func dump<T: TextOutputStream>(skipUnchanged: Bool, to: inout T)

  /// Dump the diff between the current state and a different state of all
  /// counters.
  func dumpDiff<T: TextOutputStream>(
    _ after: Self, skipUnchanged: Bool, to: inout T
  )

  /// Compute a diff between two states of runtime function counters.
  /// Return a new state representing the diff.
  func diff(_ other: Self) -> Self

  /// Access counters by name.
  subscript(_ counterName: String) -> UInt32 { get set }

  /// Access counters by index.
  subscript(_ index: Int) -> UInt32 { get set }
}

extension _RuntimeFunctionCountersStats {
  /// Dump the current state of all counters.
  public func dump(skipUnchanged: Bool) {
    var output = _Stdout()
    dump(skipUnchanged: skipUnchanged, to: &output)
  }

  /// Dump the diff between the current state and a different state of all
  /// counters.
  public func dumpDiff(_ after: Self, skipUnchanged: Bool) {
    var output = _Stdout()
    dumpDiff(after, skipUnchanged: skipUnchanged, to: &output)
  }
}

extension _RuntimeFunctionCountersStats {
  public var debugDescription: String {
    var result = ""
    dump(skipUnchanged: true, to: &result)
    return result
  }
}

// A helper type that encapsulates the logic for collecting runtime function
// counters. This type should not be used directly. You should use its
// wrappers GlobalRuntimeFunctionCountersState and
// ObjectRuntimeFunctionCountersState instead.
internal struct _RuntimeFunctionCountersState: _RuntimeFunctionCountersStats {
  /// Reserve enough space for 64 elements.
  typealias Counters =
  (
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32, UInt32,
    UInt32, UInt32, UInt32, UInt32
  )

  private var counters: Counters = (
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0), UInt32(0),
    UInt32(0), UInt32(0), UInt32(0), UInt32(0)
  )

  // Use counter name as index.
  subscript(_ counterName: String) -> UInt32 {
    get {
      if let index = _RuntimeFunctionCounters.runtimeFunctionNameToIndex[counterName] {
        return self[index]
      }
      fatalError("Unknown counter name: \(counterName)")
    }

    set {
      if let index = _RuntimeFunctionCounters.runtimeFunctionNameToIndex[counterName] {
        self[index] = newValue
        return
      }
      fatalError("Unknown counter name: \(counterName)")
    }
  }

  subscript(_ index: Int) -> UInt32 {
    @inline(never)
    get {
      if (index >= _RuntimeFunctionCounters.numRuntimeFunctionCounters) {
        fatalError("Counter index should be in the range " +
          "0..<\(_RuntimeFunctionCounters.numRuntimeFunctionCounters)")
      }
      var tmpCounters = counters
      let counter: UInt32 = withUnsafePointer(to: &tmpCounters) { ptr in
        return ptr.withMemoryRebound(to: UInt32.self, capacity: 64) { buf in
          return buf[index]
        }
      }
      return counter
    }

    @inline(never)
    set {
      if (index >= _RuntimeFunctionCounters.numRuntimeFunctionCounters) {
        fatalError("Counter index should be in the range " +
          "0..<\(_RuntimeFunctionCounters.numRuntimeFunctionCounters)")
      }
      withUnsafeMutablePointer(to: &counters) {
        $0.withMemoryRebound(to: UInt32.self, capacity: 64) {
          $0[index] = newValue
        }
      }
    }
  }
}

extension _RuntimeFunctionCounters {
  @_silgen_name("_swift_getObjectRuntimeFunctionCounters")
  internal static func getObjectRuntimeFunctionCounters(
    _ object: UnsafeRawPointer, _ result: inout _RuntimeFunctionCountersState)

  @_silgen_name("_swift_getGlobalRuntimeFunctionCounters")
  internal static func getGlobalRuntimeFunctionCounters(
    _ result: inout _RuntimeFunctionCountersState)

  @_silgen_name("_swift_setGlobalRuntimeFunctionCounters")
  internal static func setGlobalRuntimeFunctionCounters(
    _ state: inout _RuntimeFunctionCountersState)

  @_silgen_name("_swift_setObjectRuntimeFunctionCounters")
  internal static func setObjectRuntimeFunctionCounters(
    _ object: UnsafeRawPointer,
    _ state: inout _RuntimeFunctionCountersState)

  @discardableResult
  @_silgen_name("_swift_setGlobalRuntimeFunctionCountersMode")
  static
  public // @testable
  func setGlobalRuntimeFunctionCountersMode(enable: Bool) -> Bool

  @discardableResult
  @_silgen_name("_swift_setPerObjectRuntimeFunctionCountersMode")
  static
  public // @testable
  func setPerObjectRuntimeFunctionCountersMode(enable: Bool) -> Bool

  /// Enable runtime function counters updates by the runtime.
  static
  public // @testable
  func enableRuntimeFunctionCountersUpdates(
    mode: (globalMode: Bool, perObjectMode: Bool) = (true, true)) {
      _RuntimeFunctionCounters.setGlobalRuntimeFunctionCountersMode(
        enable: mode.globalMode)
      _RuntimeFunctionCounters.setPerObjectRuntimeFunctionCountersMode(
        enable: mode.perObjectMode)
  }

  /// Disable runtime function counters updates by the runtime.
  static
  public // @testable
  func disableRuntimeFunctionCountersUpdates() ->
    (globalMode: Bool, perObjectMode: Bool) {
      let oldGlobalMode =
        _RuntimeFunctionCounters.setGlobalRuntimeFunctionCountersMode(
          enable: false)
      let oldPerObjectMode =
        _RuntimeFunctionCounters.setPerObjectRuntimeFunctionCountersMode(
          enable: false)
      return (oldGlobalMode, oldPerObjectMode)
  }
}

extension _RuntimeFunctionCountersStats {
  typealias Counters = _RuntimeFunctionCounters
  @inline(never)
  public // @testable
  func dump<T: TextOutputStream>(skipUnchanged: Bool, to: inout T) {
    for i in 0..<Counters.numRuntimeFunctionCounters {
      if skipUnchanged && self[i] == 0 {
        continue
      }
      print("counter \(i) : " +
        "\(Counters.runtimeFunctionNames[i])" +
        " at offset: " +
        "\(Counters.runtimeFunctionCountersOffsets[i]):" +
        "  \(self[i])", to: &to)
    }
  }

  @inline(never)
  public // @testable
  func dumpDiff<T: TextOutputStream>(
    _ after: Self, skipUnchanged: Bool, to: inout T
  ) {
    for i in 0..<Counters.numRuntimeFunctionCounters {
      if self[i] == 0 && after[i] == 0 {
        continue
      }
      if skipUnchanged && self[i] == after[i] {
        continue
      }
      print("counter \(i) : " +
        "\(Counters.runtimeFunctionNames[i])" +
        " at offset: " +
        "\(Counters.runtimeFunctionCountersOffsets[i]): " +
        "before \(self[i]) " +
        "after \(after[i])" + " diff=\(after[i]-self[i])", to: &to)
    }
  }

  public // @testable
  func diff(_ other: Self) -> Self {
    var result = Self()
    for i in 0..<Counters.numRuntimeFunctionCounters {
      result[i] = other[i] - self[i]
    }
    return result
  }
}

/// This type should be used to collect statistics about the global runtime
/// function pointers.
public // @testable
struct _GlobalRuntimeFunctionCountersState: _RuntimeFunctionCountersStats {
  var state = _RuntimeFunctionCountersState()

  public init() {
    getGlobalRuntimeFunctionCounters()
  }

  mutating public func getGlobalRuntimeFunctionCounters() {
    _RuntimeFunctionCounters.getGlobalRuntimeFunctionCounters(&state)
  }

  mutating public func setGlobalRuntimeFunctionCounters() {
    _RuntimeFunctionCounters.setGlobalRuntimeFunctionCounters(&state)
  }

  public subscript(_ index: String) -> UInt32 {
    get {
      return state[index]
    }
    set {
      state[index] = newValue
    }
  }

  public subscript(_ index: Int) -> UInt32 {
    get {
      return state[index]
    }
    set {
      state[index] = newValue
    }
  }
}

/// This type should be used to collect statistics about object runtime
/// function pointers.
public // @testable
struct _ObjectRuntimeFunctionCountersState: _RuntimeFunctionCountersStats {
  var state = _RuntimeFunctionCountersState()

  // Initialize with the counters for a given object.
  public init(_ p: UnsafeRawPointer) {
    getObjectRuntimeFunctionCounters(p)
  }

  public init() {
  }

  mutating public func getObjectRuntimeFunctionCounters(_ o: UnsafeRawPointer) {
    _RuntimeFunctionCounters.getObjectRuntimeFunctionCounters(o, &state)
  }

  mutating public func setObjectRuntimeFunctionCounters(_ o: UnsafeRawPointer) {
    _RuntimeFunctionCounters.setObjectRuntimeFunctionCounters(o, &state)
  }

  public subscript(_ index: String) -> UInt32 {
    get {
      return state[index]
    }
    set {
      state[index] = newValue
    }
  }

  public subscript(_ index: Int) -> UInt32 {
    get {
      return state[index]
    }
    set {
      state[index] = newValue
    }
  }
}

/// Collects all references inside an object.
/// Runtime counters tracking is disabled for the duration of this operation
/// so that it does not affect those counters.
public // @testable
func _collectReferencesInsideObject(_ value: Any) -> [UnsafeRawPointer] {
  let savedMode = _RuntimeFunctionCounters.disableRuntimeFunctionCountersUpdates()
  // Collect all references inside the object
  let refs = _RuntimeFunctionCounters.collectAllReferencesInsideObject(value)
  _RuntimeFunctionCounters.enableRuntimeFunctionCountersUpdates(mode: savedMode)
  return refs
}

/// A helper method to measure how global and per-object function counters
/// were changed during execution of a closure provided as a parameter.
/// Returns counter diffs for global counters and for the object-specific
/// counters related to a given object.
public // @testable
func _measureRuntimeFunctionCountersDiffs(
  objects: [UnsafeRawPointer], _ body: () -> Void) ->
  (_GlobalRuntimeFunctionCountersState, [_ObjectRuntimeFunctionCountersState]) {
    let savedMode =
      _RuntimeFunctionCounters.disableRuntimeFunctionCountersUpdates()
    let globalCountersBefore = _GlobalRuntimeFunctionCountersState()
    var objectsCountersBefore: [_ObjectRuntimeFunctionCountersState] = []
    for object in objects {
      objectsCountersBefore.append(_ObjectRuntimeFunctionCountersState(object))
    }
    // Enable counters updates.
    _RuntimeFunctionCounters.enableRuntimeFunctionCountersUpdates(
      mode: (globalMode: true, perObjectMode: true))
    // Execute the provided user's code.
    body()
    // Disable counters updates.
    _RuntimeFunctionCounters.enableRuntimeFunctionCountersUpdates(
    mode: (globalMode: false, perObjectMode: false))

    let globalCountersAfter = _GlobalRuntimeFunctionCountersState()
    var objectsCountersDiff: [_ObjectRuntimeFunctionCountersState] = []
    for (idx, object) in objects.enumerated() {
      let objectCountersAfter = _ObjectRuntimeFunctionCountersState(object)
      objectsCountersDiff.append(
        objectsCountersBefore[idx].diff(objectCountersAfter))
    }

    _RuntimeFunctionCounters.enableRuntimeFunctionCountersUpdates(
      mode: savedMode)
    return (globalCountersBefore.diff(globalCountersAfter), objectsCountersDiff)
}

#endif
