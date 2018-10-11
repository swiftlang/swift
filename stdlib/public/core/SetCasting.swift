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
  var builder = _SetBuilder<BaseValue>(count: source.count)
  for x in source {
    builder.add(member: x as! BaseValue)
  }
  return builder.take()
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
  return _setDownCastConditional(source)!
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
  var result = Set<DerivedValue>(minimumCapacity: source.count)
  for member in source {
    if let derivedMember = member as? DerivedValue {
      result.insert(derivedMember)
      continue
    }
    return nil
  }
  return result
}
