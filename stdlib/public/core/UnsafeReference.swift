//===--- UnsafeReference.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Holds an instance of `Object`, carrying ownership semantics that
/// are not known to the type system and not represented in memory.
///
/// `UnsafeReference<T>` appears as a return type or "out" parameter
/// in [Core
/// Foundation](https://developer.apple.com/library/mac/documentation/CoreFoundation/Reference/CoreFoundation_Collection/)
/// APIs that have not been
/// [annotated](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/BuildingCocoaApps/WorkingWithCocoaDataTypes.html#//apple_ref/doc/uid/TP40014216-CH6-ID79)
/// with information that allows clients to receive a safe `T`
/// directly.
///
/// An `UnsafeReference` instance `u` can be in one of three
/// "ownership states":
///
///  1. **Unretained**, where `u.object` yields a valid `T` and will
///     do so through any number of accesses to the `.object`
///     properties of `UnsafeReference` instances.  The behavior of
///     `u.release()` is undefined, and any other operations may cause
///     `u` to transition to the *released* state.
///
///  2. **Retained**, where `u.release()` yields a valid `T` and will
///     do so exactly once.  Calling `.release()` transitions `u` and
///     all its copies to the *released* state.
///
///  3. **Released**, where the behavior of both `u.object` and
///     `u.release()` is undefined.  A released `UnsafeReference`
///     can't be used for anything.
///
/// The ownership state of an `UnsafeReference` is not
/// programmatically detectable, so careful documentation is
/// essential.  When an `UnsafeReference` is returned in the
/// *retained* state, it is usual to document that "the caller is
/// responsible for releasing the object" or that the API "follows
/// the [create
/// rule](https://developer.apple.com/library/ios/documentation/CoreFoundation/Conceptual/CFMemoryMgmt/Concepts/Ownership.html#//apple_ref/doc/writerid/cfCreateRule)."
/// Other `UnsafeReferences` are assumed to be in the *unretained*
/// state.  No API should pass or return a *released*
/// `UnsafeReference`
///
/// The safest way to deal with an instance of `UnsafeReference<T>` is
/// to immediately extract a safe `T` from it exactly once (via
/// `.object` or `.release()` according to its state), and let it go
/// out of scope.
///
/// In the common case where the `UnsafeReference` is a return value,
/// it's best to do the extraction as part of the call, e.g.:
/// ~~~~
/// let names: CFArray = CFHostGetNames(host).object
/// let url: CFURL = CFHTTPMessageCopyRequestURL(message).release()
/// ~~~
///
/// When the `UnsafeReference` is an "out" parameter, you can limit
/// its scope by creating and unwrapping it in a closure:
/// ~~~~
/// var properties: CFPropertyList = try {
///   var properties: UnsafeReference<CFPropertyList>?
///   let error = MIDIObjectGetProperties(midiClient, &properties, true)
///   if error != noErr {
///     throw NSError(domain: "midi", code: Int(error), userInfo: nil)
///   }
///   return properties!.object
/// }()
/// ~~~~
public struct UnsafeReference<Object: AnyObject> {
  
  /// Relinquishes ownership of the `Object` and returns it as a safe
  /// reference.
  ///
  /// - Requires: `self` is in the *retained* state.
  /// 
  /// - Postcondition: `self` and all its copies are in the *released* state.
  ///
  /// - Warning: Calling `.release()` on an *unretained* or *released*
  ///   `UnsafeReference` is a severe programming error yielding
  ///   undefined behavior.
  /// 
  /// - Warning: After this method is invoked once, invoking any
  ///   methods on the same instance, or a copy thereof, is a severe
  ///   programming error yielding undefined behavior.
  public func release() -> Object {
    defer { Builtin.release(_storage) }
    return self.object
  }
  
  /// A safe reference to the `Object` instance.
  ///
  /// - Warning: if `self` is in the *retained* state, you must
  ///   eventually call `.release()`, or the resulting object will be
  ///   leaked.  It's better to just capture the result of invoking
  ///   `.release()` in that case.
  public var object: Object {
    return _storage
  }

  /// Creates an unsafe holder of `safeObject` in the *unretained*
  /// state; the held object can be accessed via the `.object` property.
  public init(withoutRetaining safeObject: Object) {
    _storage = safeObject
  }
  
  /// Creates an unsafe holder of `safeObject` in the *retained*
  /// state; the held object can be accessed via the `release()`
  /// method.
  public init(retaining safeObject: Object) {
    self.init(withoutRetaining: safeObject)
    Builtin.retain(_storage)
  }
  

  /// Creates an unsafe holder of an object having the given
  /// `bitPattern`.
  public init(bitPattern: OpaquePointer) {
    _stdlibAssert(
      bitPattern != nil,
      "attempt to create an UnsafeReference from a null pointer")
    self.init(withoutRetaining: unsafeBitCast(bitPattern, Object.self))
  }
  
  internal unowned(unsafe) var _storage: Object
}

@available(*, unavailable, renamed="UnsafeReference")
public struct Unmanaged<Instance : AnyObject> {
  @available(*, unavailable, message="use the 'UnsafeReference(bitPattern:)' initializer")
  public static func fromOpaque(value: OpaquePointer) -> UnsafeReference<Instance> {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, message="use the 'OpaquePointer(bitPattern:)' initializer")
  public func toOpaque() -> OpaquePointer {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, message="use the 'UnsafeReference(withoutRetaining:)' initializer")
  public static func passRetained(value: Instance) -> UnsafeReference<Instance> {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed="object")
  public func takeUnretainedValue() -> Instance {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, renamed="release")
  public func takeRetainedValue() -> Instance {
    fatalError("unavailable function can't be called")
  }
}

