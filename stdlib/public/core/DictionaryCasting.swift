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

//===--- Compiler conversion/casting entry points for Dictionary<K, V> ----===//

extension Dictionary {
  @_alwaysEmitIntoClient @inlinable // Introduced in 5.1
  @inline(__always)
  internal init?<C: Collection>(
    _mapping source: C,
    allowingDuplicates: Bool,
    transform: (C.Element) -> (key: Key, value: Value)?
  ) {
    var target = _NativeDictionary<Key, Value>(capacity: source.count)
    if allowingDuplicates {
      for member in source {
        guard let (key, value) = transform(member) else { return nil }
        target._unsafeUpdate(key: key, value: value)
      }
    } else {
      for member in source {
        guard let (key, value) = transform(member) else { return nil }
        target._unsafeInsertNew(key: key, value: value)
      }
    }
    self.init(_native: target)
  }
}

/// Perform a non-bridged upcast that always succeeds.
///
/// - Precondition: `BaseKey` and `BaseValue` are base classes or base `@objc`
///   protocols (such as `AnyObject`) of `DerivedKey` and `DerivedValue`,
///   respectively.
@inlinable
@_unavailableInEmbedded
public func _dictionaryUpCast<DerivedKey, DerivedValue, BaseKey, BaseValue>(
    _ source: Dictionary<DerivedKey, DerivedValue>
) -> Dictionary<BaseKey, BaseValue> {
  return Dictionary(
    _mapping: source,
    // String and NSString have different concepts of equality, so
    // NSString-keyed Dictionaries may generate key collisions when "upcasted"
    // to String. See rdar://problem/35995647
    allowingDuplicates: (BaseKey.self == String.self)
  ) { k, v in
    (k as! BaseKey, v as! BaseValue)
  }!
}

/// Called by the casting machinery.
@_silgen_name("_swift_dictionaryDownCastIndirect")
@_unavailableInEmbedded
internal func _dictionaryDownCastIndirect<SourceKey, SourceValue,
                                          TargetKey, TargetValue>(
  _ source: UnsafePointer<Dictionary<SourceKey, SourceValue>>,
  _ target: UnsafeMutablePointer<Dictionary<TargetKey, TargetValue>>) {
  unsafe target.initialize(to: _dictionaryDownCast(source.pointee))
}

/// Implements a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
///
/// - Precondition: `DerivedKey` is a subtype of `BaseKey`, `DerivedValue` is
///   a subtype of `BaseValue`, and all of these types are reference types.
@inlinable
@_unavailableInEmbedded
public func _dictionaryDownCast<BaseKey, BaseValue, DerivedKey, DerivedValue>(
  _ source: Dictionary<BaseKey, BaseValue>
) -> Dictionary<DerivedKey, DerivedValue> {

#if _runtime(_ObjC)
  if _isClassOrObjCExistential(BaseKey.self)
  && _isClassOrObjCExistential(BaseValue.self)
  && _isClassOrObjCExistential(DerivedKey.self)
  && _isClassOrObjCExistential(DerivedValue.self) {

    guard source._variant.isNative else {
      return Dictionary(
        _immutableCocoaDictionary: source._variant.asCocoa.object)
    }
    // Note: it is safe to treat the buffer as immutable here because
    // Dictionary will not mutate buffer with reference count greater than 1.
    return Dictionary(
      _immutableCocoaDictionary: source._variant.asNative.bridged())
  }
#endif

  // Note: We can't delegate this call to _dictionaryDownCastConditional,
  // because we rely on as! to generate nice runtime errors when the downcast
  // fails.

  return Dictionary(
    _mapping: source,
    // String and NSString have different concepts of equality, so
    // NSString-keyed Dictionaries may generate key collisions when downcasted
    // to String. See rdar://problem/35995647
    allowingDuplicates: (DerivedKey.self == String.self)
  ) { k, v in
    (k as! DerivedKey, v as! DerivedValue)
  }!
}

/// Called by the casting machinery.
@_silgen_name("_swift_dictionaryDownCastConditionalIndirect")
@_unavailableInEmbedded
internal func _dictionaryDownCastConditionalIndirect<SourceKey, SourceValue,
                                                     TargetKey, TargetValue>(
  _ source: UnsafePointer<Dictionary<SourceKey, SourceValue>>,
  _ target: UnsafeMutablePointer<Dictionary<TargetKey, TargetValue>>
) -> Bool {
  if let result: Dictionary<TargetKey, TargetValue>
       = unsafe _dictionaryDownCastConditional(source.pointee) {
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
/// - Precondition: `DerivedKey` is a subtype of `BaseKey`, `DerivedValue` is
///   a subtype of `BaseValue`, and all of these types are reference types.
@inlinable
@_unavailableInEmbedded
public func _dictionaryDownCastConditional<
  BaseKey, BaseValue, DerivedKey, DerivedValue
>(
  _ source: Dictionary<BaseKey, BaseValue>
) -> Dictionary<DerivedKey, DerivedValue>? {
  return Dictionary(
    _mapping: source,
    // String and NSString have different concepts of equality, so
    // NSString-keyed Dictionaries may generate key collisions when downcasted
    // to String. See rdar://problem/35995647
    allowingDuplicates: (DerivedKey.self == String.self)
  ) { k, v in
    guard
      let key = k as? DerivedKey,
      let value = v as? DerivedValue
    else {
      return nil
    }
    return (key, value)
  }
}
