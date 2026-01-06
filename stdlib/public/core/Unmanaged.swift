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

/// A type for propagating an unmanaged object reference.
///
/// When you use this type, you become partially responsible for
/// keeping the object alive.
@frozen
@unsafe
public struct Unmanaged<Instance: AnyObject> {
  @usableFromInline
  internal unowned(unsafe) var _value: Instance

  @usableFromInline @_transparent
  internal init(_private: Instance) { unsafe _value = _private }

  /// Unsafely turns an opaque C pointer into an unmanaged class reference.
  ///
  /// This operation does not change reference counts.
  ///
  ///     let str: CFString = Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  ///
  /// - Parameter value: An opaque C pointer.
  /// - Returns: An unmanaged class reference to `value`.
  @_transparent
  public static func fromOpaque(
    @_nonEphemeral _ value: UnsafeRawPointer
  ) -> Unmanaged {
    // NOTE: `value` is allowed to represent a dangling reference, so 
    // this function must not ever try to dereference it. For
    // example, this function must NOT use the init(_private:) initializer
    // because doing so requires materializing a strong reference to 'Instance'.
    // This materialization would be enough to convince the compiler to add
    // retain/releases which must be avoided for the opaque pointer functions.
    // 'Unmanaged<Instance>' is layout compatible with 'UnsafeRawPointer' and
    // casting to that will not attempt to retain the reference held at 'value'.
    unsafe unsafeBitCast(value, to: Unmanaged<Instance>.self)
  }

  /// Unsafely converts an unmanaged class reference to a pointer.
  ///
  /// This operation does not change reference counts.
  ///
  ///     let str0 = "boxcar" as CFString
  ///     let bits = Unmanaged.passUnretained(str0)
  ///     let ptr = bits.toOpaque()
  ///
  /// - Returns: An opaque pointer to the value of this unmanaged reference.
  @_transparent
  public func toOpaque() -> UnsafeMutableRawPointer {
    // NOTE: `self` is allowed to be a dangling reference.
    // Therefore, this function must not unsafeBitCast '_value' because
    // that will get a strong reference temporary value that the compiler will
    // try to retain/release. Use 'self' to avoid this. 'Unmanaged<Instance>' is
    // layout compatible with 'UnsafeRawPointer' and casting from that will not
    // attempt to retain the reference held at '_value'.
    unsafe unsafeBitCast(self, to: UnsafeMutableRawPointer.self)
  }

  /// Creates an unmanaged reference with an unbalanced retain.
  ///
  /// The instance passed as `value` will leak if nothing eventually balances
  /// the retain.
  ///
  /// This is useful when passing an object to an API which Swift does not know
  /// the ownership rules for, but you know that the API expects you to pass
  /// the object at +1.
  ///
  /// - Parameter value: A class instance.
  /// - Returns: An unmanaged reference to the object passed as `value`.
  @_transparent
  public static func passRetained(_ value: Instance) -> Unmanaged {
    // Retain 'value' before it becomes unmanaged. This may be its last use.
    Builtin.retain(value)
    return unsafe Unmanaged(_private: value)
  }

  /// Creates an unmanaged reference without performing an unbalanced
  /// retain.
  ///
  /// This is useful when passing a reference to an API which Swift
  /// does not know the ownership rules for, but you know that the
  /// API expects you to pass the object at +0.
  ///
  ///     CFArraySetValueAtIndex(.passUnretained(array), i,
  ///                            .passUnretained(object))
  ///
  /// - Parameter value: A class instance.
  /// - Returns: An unmanaged reference to the object passed as `value`.
  @_transparent
  public static func passUnretained(_ value: Instance) -> Unmanaged {
    return unsafe Unmanaged(_private: value)
  }

  /// Gets the value of this unmanaged reference as a managed
  /// reference without consuming an unbalanced retain of it.
  ///
  /// This is useful when a function returns an unmanaged reference
  /// and you know that you're not responsible for releasing the result.
  ///
  /// - Returns: The object referenced by this `Unmanaged` instance.
  @_transparent // unsafe-performance
  public func takeUnretainedValue() -> Instance {
    return unsafe _value
  }

  /// Gets the value of this unmanaged reference as a managed
  /// reference and consumes an unbalanced retain of it.
  ///
  /// This is useful when a function returns an unmanaged reference
  /// and you know that you're responsible for releasing the result.
  ///
  /// - Returns: The object referenced by this `Unmanaged` instance.
  @_transparent // unsafe-performance
  public func takeRetainedValue() -> Instance {
    let result = unsafe _value
    unsafe release()
    return result
  }

  /// Gets the value of the unmanaged referenced as a managed reference without
  /// consuming an unbalanced retain of it and passes it to the closure. Asserts
  /// that there is some other reference ('the owning reference') to the
  /// instance referenced by the unmanaged reference that guarantees the
  /// lifetime of the instance for the duration of the
  /// '_withUnsafeGuaranteedRef' call.
  ///
  /// NOTE: You are responsible for ensuring this by making the owning
  /// reference's lifetime fixed for the duration of the
  /// '_withUnsafeGuaranteedRef' call.
  ///
  /// Violation of this will incur undefined behavior.
  ///
  /// A lifetime of a reference 'the instance' is fixed over a point in the
  /// program if:
  ///
  /// * There exists a global variable that references 'the instance'.
  ///
  ///   import Foundation
  ///   var globalReference = Instance()
  ///   func aFunction() {
  ///      point()
  ///   }
  ///
  /// Or if:
  ///
  /// * There is another managed reference to 'the instance' whose life time is
  ///   fixed over the point in the program by means of 'withExtendedLifetime'
  ///   dynamically closing over this point.
  ///
  ///   var owningReference = Instance()
  ///   ...
  ///   withExtendedLifetime(owningReference) {
  ///       point($0)
  ///   }
  ///
  /// Or if:
  ///
  /// * There is a class, or struct instance ('owner') whose lifetime is fixed
  ///   at the point and which has a stored property that references
  ///   'the instance' for the duration of the fixed lifetime of the 'owner'.
  ///
  ///  class Owned {
  ///  }
  ///
  ///  class Owner {
  ///    final var owned: Owned
  ///
  ///    func foo() {
  ///        withExtendedLifetime(self) {
  ///            doSomething(...)
  ///        } // Assuming: No stores to owned occur for the dynamic lifetime of
  ///          //           the withExtendedLifetime invocation.
  ///    }
  ///
  ///    func doSomething() {
  ///       // both 'self' and 'owned''s lifetime is fixed over this point.
  ///       point(self, owned)
  ///    }
  ///  }
  ///
  /// The last rule applies transitively through a chain of stored references
  /// and nested structs.
  ///
  /// Examples:
  ///
  ///   var owningReference = Instance()
  ///   ...
  ///   withExtendedLifetime(owningReference) {
  ///     let u = Unmanaged.passUnretained(owningReference)
  ///     for i in 0 ..< 100 {
  ///       u._withUnsafeGuaranteedRef {
  ///         $0.doSomething()
  ///       }
  ///     }
  ///   }
  ///
  ///  class Owner {
  ///    final var owned: Owned
  ///
  ///    func foo() {
  ///        withExtendedLifetime(self) {
  ///            doSomething(Unmanaged.passUnretained(owned))
  ///        }
  ///    }
  ///
  ///    func doSomething(_ u: Unmanaged<Owned>) {
  ///      u._withUnsafeGuaranteedRef {
  ///        $0.doSomething()
  ///      }
  ///    }
  ///  }
  @inlinable // unsafe-performance
  @_transparent
  public func _withUnsafeGuaranteedRef<Result>(
    _ body: (Instance) throws -> Result
  ) rethrows -> Result {
    var tmp = unsafe self
    // Builtin.convertUnownedUnsafeToGuaranteed expects to have a base value
    // that the +0 value depends on. In this case, we are assuming that is done
    // for us opaquely already. So, the builtin will emit a mark_dependence on a
    // trivial object. The optimizer knows to eliminate that so we do not have
    // any overhead from this.
    let fakeBase: Int? = nil
    return try unsafe body(Builtin.convertUnownedUnsafeToGuaranteed(fakeBase,
                                                             &tmp._value))
  }

  /// Performs an unbalanced retain of the object.
  @_transparent
  public func retain() -> Unmanaged {
    unsafe Builtin.retain(_value)
    return unsafe self
  }

  /// Performs an unbalanced release of the object.
  @_transparent
  public func release() {
    unsafe Builtin.release(_value)
  }

#if _runtime(_ObjC)
  /// Performs an unbalanced autorelease of the object.
  @_transparent
  public func autorelease() -> Unmanaged {
    unsafe Builtin.autorelease(_value)
    return unsafe self
  }
#endif
}

extension Unmanaged: Sendable where Instance: Sendable { }

