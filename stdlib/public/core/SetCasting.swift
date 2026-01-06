//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===--- Compiler conversion/casting entry points for Set<Element> --------===//

extension Set {
  @_alwaysEmitIntoClient @inlinable // Introduced in 5.1
  @inline(__always)
  internal init?<C: Collection>(
    _mapping source: C,
    allowingDuplicates: Bool,
    transform: (C.Element) -> Element?
  ) {
    var target = _NativeSet<Element>(capacity: source.count)
    if allowingDuplicates {
      for m in source {
        guard let member = transform(m) else { return nil }
        target._unsafeUpdate(with: member)
      }
    } else {
      for m in source {
        guard let member = transform(m) else { return nil }
        unsafe target._unsafeInsertNew(member)
      }
    }
    self.init(_native: target)
  }
}

/// Perform a non-bridged upcast that always succeeds.
///
/// - Precondition: `BaseValue` is a base class or base `@objc`
///   protocol (such as `AnyObject`) of `DerivedValue`.
@inlinable
@_unavailableInEmbedded
public func _setUpCast<DerivedValue, BaseValue>(
  _ source: Set<DerivedValue>
) -> Set<BaseValue> {
  return Set(
    _mapping: source,
    // String and NSString have different concepts of equality, so Set<NSString>
    // may generate key collisions when "upcasted" to Set<String>.
    // See rdar://problem/35995647
    allowingDuplicates: (BaseValue.self == String.self)
  ) { member in
    (member as! BaseValue)
  }!
}

/// Called by the casting machinery.
@_silgen_name("_swift_setDownCastIndirect")
@_unavailableInEmbedded
internal func _setDownCastIndirect<SourceValue, TargetValue>(
  _ source: UnsafePointer<Set<SourceValue>>,
  _ target: UnsafeMutablePointer<Set<TargetValue>>) {
  unsafe target.initialize(to: _setDownCast(source.pointee))
}

/// Implements a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
///
/// - Precondition: `DerivedValue` is a subtype of `BaseValue` and both
///   are reference types.
@inlinable
@_unavailableInEmbedded
public func _setDownCast<BaseValue, DerivedValue>(_ source: Set<BaseValue>)
  -> Set<DerivedValue> {

#if _runtime(_ObjC)
  if _isClassOrObjCExistential(BaseValue.self)
  && _isClassOrObjCExistential(DerivedValue.self) {
    guard source._variant.isNative else {
      return Set(_immutableCocoaSet: source._variant.asCocoa.object)
    }
    return Set(_immutableCocoaSet: source._variant.asNative.bridged())
  }
#endif
  // We can't just delegate to _setDownCastConditional here because we rely on
  // `as!` to generate nice runtime errors when the downcast fails.

  return Set(
    _mapping: source,
    // String and NSString have different concepts of equality, so
    // NSString-keyed Sets may generate key collisions when downcasted
    // to String. See rdar://problem/35995647
    allowingDuplicates: (DerivedValue.self == String.self)
  ) { member in
    (member as! DerivedValue)
  }!
}

/// Called by the casting machinery.
@_silgen_name("_swift_setDownCastConditionalIndirect")
@_unavailableInEmbedded
internal func _setDownCastConditionalIndirect<SourceValue, TargetValue>(
  _ source: UnsafePointer<Set<SourceValue>>,
  _ target: UnsafeMutablePointer<Set<TargetValue>>
) -> Bool {
  if let result: Set<TargetValue> = unsafe _setDownCastConditional(source.pointee) {
    unsafe target.initialize(to: result)
    return true
  }
  return false
}

/// Implements a conditional downcast.
///
/// If the cast fails, the function returns `nil`.  All checks should be
/// performed eagerly.
///
/// - Precondition: `DerivedValue` is a subtype of `BaseValue` and both
///   are reference types.
@inlinable
@_unavailableInEmbedded
public func _setDownCastConditional<BaseValue, DerivedValue>(
  _ source: Set<BaseValue>
) -> Set<DerivedValue>? {
  return Set(
    _mapping: source,
    // String and NSString have different concepts of equality, so
    // NSString-keyed Sets may generate key collisions when downcasted
    // to String. See rdar://problem/35995647
    allowingDuplicates: (DerivedValue.self == String.self)
  ) { member in
    member as? DerivedValue
  }
}
