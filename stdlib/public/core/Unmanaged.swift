//===----------------------------------------------------------------------===//
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

/// A type for propagating an unmanaged object reference.
///
/// When you use this type, you become partially responsible for
/// keeping the object alive.
@_fixed_layout
public struct Unmanaged<Instance : AnyObject> {
  internal unowned(unsafe) var _value: Instance

  @_versioned
  @_transparent
  internal init(_private: Instance) { _value = _private }

  /// Unsafely turn an opaque C pointer into an unmanaged
  /// class reference.
  ///
  /// This operation does not change reference counts.
  ///
  ///     let str: CFString = Unmanaged.fromOpaque(ptr).takeUnretainedValue()
  @_transparent
  @warn_unused_result
  public static func fromOpaque(_ value: OpaquePointer) -> Unmanaged {
    return Unmanaged(_private: unsafeBitCast(value, to: Instance.self))
  }

  /// Create an unmanaged reference with an unbalanced retain.
  /// The object will leak if nothing eventually balances the retain.
  ///
  /// This is useful when passing an object to an API which Swift
  /// does not know the ownership rules for, but you know that the
  /// API expects you to pass the object at +1.
  @_transparent
  @warn_unused_result
  public static func passRetained(_ value: Instance) -> Unmanaged {
    return Unmanaged(_private: value).retain()
  }

  /// Create an unmanaged reference without performing an unbalanced
  /// retain.
  ///
  /// This is useful when passing a reference to an API which Swift
  /// does not know the ownership rules for, but you know that the
  /// API expects you to pass the object at +0.
  ///
  ///     CFArraySetValueAtIndex(.passUnretained(array), i,
  ///                            .passUnretained(object))
  @_transparent
  @warn_unused_result
  public static func passUnretained(_ value: Instance) -> Unmanaged {
    return Unmanaged(_private: value)
  }

  /// Get the value of this unmanaged reference as a managed
  /// reference without consuming an unbalanced retain of it.
  ///
  /// This is useful when a function returns an unmanaged reference
  /// and you know that you're not responsible for releasing the result.
  @warn_unused_result
  public func takeUnretainedValue() -> Instance {
    return _value
  }

  /// Get the value of this unmanaged reference as a managed
  /// reference and consume an unbalanced retain of it.
  ///
  /// This is useful when a function returns an unmanaged reference
  /// and you know that you're responsible for releasing the result.
  @warn_unused_result
  public func takeRetainedValue() -> Instance {
    let result = _value
    release()
    return result
  }

  /// Get the value of the unmanaged referenced as a managed reference without
  /// consuming an unbalanced retain of it and pass it to the closure. Asserts
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
  /// programm if:
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
  ///    final var owned : Owned
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
  /// The last rule applies transitively through a chains of stored references
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
  ///    final var owned : Owned
  ///
  ///    func foo() {
  ///        withExtendedLifetime(self) {
  ///            doSomething(Unmanaged.passUnretained(owned))
  ///        }
  ///    }
  ///
  ///    func doSomething(_ u : Unmanaged<Owned>) {
  ///      u._withUnsafeGuaranteedRef {
  ///        $0.doSomething()
  ///      }
  ///    }
  ///  }
  public func _withUnsafeGuaranteedRef<Result>(
    _ closure: @noescape (Instance) throws -> Result
  ) rethrows -> Result {
    let (guaranteedInstance, token) = Builtin.unsafeGuaranteed(_value)
    let result = try closure(guaranteedInstance)
    Builtin.unsafeGuaranteedEnd(token)
    return result
  }

  /// Perform an unbalanced retain of the object.
  @_transparent
  public func retain() -> Unmanaged {
    Builtin.retain(_value)
    return self
  }

  /// Perform an unbalanced release of the object.
  @_transparent
  public func release() {
    Builtin.release(_value)
  }

#if _runtime(_ObjC)
  /// Perform an unbalanced autorelease of the object.
  @_transparent
  public func autorelease() -> Unmanaged {
    Builtin.autorelease(_value)
    return self
  }
#endif
}
