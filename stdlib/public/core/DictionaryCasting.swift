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

/// Perform a non-bridged upcast that always succeeds.
///
/// - Precondition: `BaseKey` and `BaseValue` are base classes or base `@objc`
///   protocols (such as `AnyObject`) of `DerivedKey` and `DerivedValue`,
///   respectively.
@inlinable
public func _dictionaryUpCast<DerivedKey, DerivedValue, BaseKey, BaseValue>(
    _ source: Dictionary<DerivedKey, DerivedValue>
) -> Dictionary<BaseKey, BaseValue> {
  var builder = _DictionaryBuilder<BaseKey, BaseValue>(count: source.count)

  for (k, v) in source {
    builder.add(key:k as! BaseKey, value: v as! BaseValue)
  }
  return builder.take()
}

/// Called by the casting machinery.
@_silgen_name("_swift_dictionaryDownCastIndirect")
internal func _dictionaryDownCastIndirect<SourceKey, SourceValue,
                                          TargetKey, TargetValue>(
  _ source: UnsafePointer<Dictionary<SourceKey, SourceValue>>,
  _ target: UnsafeMutablePointer<Dictionary<TargetKey, TargetValue>>) {
  target.initialize(to: _dictionaryDownCast(source.pointee))
}

/// Implements a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
///
/// - Precondition: `DerivedKey` is a subtype of `BaseKey`, `DerivedValue` is
///   a subtype of `BaseValue`, and all of these types are reference types.
@inlinable
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
  return _dictionaryDownCastConditional(source)!
}

/// Called by the casting machinery.
@_silgen_name("_swift_dictionaryDownCastConditionalIndirect")
internal func _dictionaryDownCastConditionalIndirect<SourceKey, SourceValue,
                                                     TargetKey, TargetValue>(
  _ source: UnsafePointer<Dictionary<SourceKey, SourceValue>>,
  _ target: UnsafeMutablePointer<Dictionary<TargetKey, TargetValue>>
) -> Bool {
  if let result: Dictionary<TargetKey, TargetValue>
       = _dictionaryDownCastConditional(source.pointee) {
    target.initialize(to: result)
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
public func _dictionaryDownCastConditional<
  BaseKey, BaseValue, DerivedKey, DerivedValue
>(
  _ source: Dictionary<BaseKey, BaseValue>
) -> Dictionary<DerivedKey, DerivedValue>? {

  var builder = _DictionaryBuilder<DerivedKey, DerivedValue>(count: source.count)
  for (k, v) in source {
    guard let k1 = k as? DerivedKey, let v1 = v as? DerivedValue
    else { return nil }
    builder.add(key: k1, value: v1)
  }
  return builder.take()
}
