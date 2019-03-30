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

/// Perform a non-bridged upcast that always succeeds.
///
/// - Precondition: `BaseValue` is a base class or base `@objc`
///   protocol (such as `AnyObject`) of `DerivedValue`.
@inlinable
public func _setUpCast<DerivedValue, BaseValue>(_ source: Set<DerivedValue>)
  -> Set<BaseValue> {
  // String and NSString have different concepts of equality, so Set<NSString>
  // may generate key collisions when "upcasted" to Set<String>.
  // See rdar://problem/35995647
  let allowDuplicates = (BaseValue.self == String.self)

  var builder = _NativeSet<BaseValue>(capacity: source.count)
  for member in source {
    _ = builder.insertWithGuaranteedCapacity(
      member as! BaseValue,
      allowingDuplicates: allowDuplicates)
  }
  return Set(_native: builder)
}

/// Called by the casting machinery.
@_silgen_name("_swift_setDownCastIndirect")
internal func _setDownCastIndirect<SourceValue, TargetValue>(
  _ source: UnsafePointer<Set<SourceValue>>,
  _ target: UnsafeMutablePointer<Set<TargetValue>>) {
  target.initialize(to: _setDownCast(source.pointee))
}

/// Implements a forced downcast.  This operation should have O(1) complexity.
///
/// The cast can fail if bridging fails.  The actual checks and bridging can be
/// deferred.
///
/// - Precondition: `DerivedValue` is a subtype of `BaseValue` and both
///   are reference types.
@inlinable
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

  // String and NSString have different concepts of equality, so
  // NSString-keyed Sets may generate key collisions when downcasted
  // to String. See rdar://problem/35995647
  let allowDuplicates = (DerivedValue.self == String.self)

  var builder = _NativeSet<DerivedValue>(capacity: source.count)
  for member in source {
    builder.insertWithGuaranteedCapacity(
      member as! DerivedValue,
      allowingDuplicates: allowDuplicates)
  }
  return Set(_native: builder)
}

/// Called by the casting machinery.
@_silgen_name("_swift_setDownCastConditionalIndirect")
internal func _setDownCastConditionalIndirect<SourceValue, TargetValue>(
  _ source: UnsafePointer<Set<SourceValue>>,
  _ target: UnsafeMutablePointer<Set<TargetValue>>
) -> Bool {
  if let result: Set<TargetValue> = _setDownCastConditional(source.pointee) {
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
/// - Precondition: `DerivedValue` is a subtype of `BaseValue` and both
///   are reference types.
@inlinable
public func _setDownCastConditional<BaseValue, DerivedValue>(
  _ source: Set<BaseValue>
) -> Set<DerivedValue>? {
  // String and NSString have different concepts of equality, so
  // NSString-keyed Sets may generate key collisions when downcasted
  // to String. See rdar://problem/35995647
  let allowDuplicates = (DerivedValue.self == String.self)

  var builder = _NativeSet<DerivedValue>(capacity: source.count)
  for member in source {
    guard let derivedMember = member as? DerivedValue else { return nil }
    _ = builder.insertWithGuaranteedCapacity(
      derivedMember,
      allowingDuplicates: allowDuplicates)
  }
  return Set(_native: builder)
}
