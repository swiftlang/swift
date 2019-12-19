//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@_silgen_name("swift_isClassType")
internal func _isClassType(_: Any.Type) -> Bool

@_silgen_name("swift_reflectionMirror_recursiveCount")
internal func _getChildCount<T>(_: T.Type) -> Int

@_silgen_name("swift_reflectionMirror_recursiveChildMetadata")
internal func _getChildMetadata<T>(
  _: T.Type,
  index: Int,
  outName: UnsafeMutablePointer<UnsafePointer<CChar>?>,
  outFreeFunc: UnsafeMutablePointer<NameFreeFunc?>
) -> Any.Type

@_silgen_name("swift_reflectionMirror_recursiveChildOffset")
internal func _getChildOffset<T>(
  _: T.Type,
  index: Int
) -> Int

internal typealias NameFreeFunc = @convention(c) (UnsafePointer<CChar>?) -> Void

/// Options for calling `forEachField(of:options:body:)`.
public struct FieldOptions: OptionSet {
  public var rawValue: UInt32
  
  public init(rawValue: UInt32) {
    self.rawValue = rawValue
  }
  
  /// Require the top-level type to be a class.
  ///
  /// If this is not set, the top-level type is required to be a struct or
  /// tuple.
  public static var classType = FieldOptions(rawValue: 1 << 0)
  
  /// Ignore fields that can't be introspected.
  ///
  /// If not set, the presence of things that can't be introspected causes
  /// the function to immediately return `false`.
  public static var ignoreUnknown = FieldOptions(rawValue: 1 << 1)
}

/// Calls the given closure on every field of the specified type.
///
/// If `body` returns `false` for any field, no additional fields are visited.
///
/// - Parameters:
///   - type: The type to inspect.
///   - options: Options to use when reflecting over `type`.
///   - body: A closure to call with information about each field in `type`.
///     The parameters to `body` are a pointer to a C string holding the name
///     of the field, the offset of the field in bytes, and the type of the
///     field.
/// - Returns: `true` if every invocation of `body` returns `true`; otherwise,
///   `false`.
@discardableResult
public func forEachField<T>(
  of type: T.Type,
  options: FieldOptions = [],
  body: (UnsafePointer<CChar>, Int, Any.Type) -> Bool
) -> Bool {
  // Require class type iff `.classType` is included as an option
  if _isClassType(T.self) != options.contains(.classType) {
    return false
  }
  
  let childCount = _getChildCount(type)
  for i in 0..<childCount {
    let offset = _getChildOffset(type, index: i)
    
    var nameC: UnsafePointer<CChar>? = nil
    var freeFunc: NameFreeFunc? = nil
    let childType = _getChildMetadata(type, index: i, outName: &nameC, outFreeFunc: &freeFunc)
    defer { freeFunc?(nameC) }
    
    if !body(nameC!, offset, childType) {
      return false
    }
  }
  
  return true
}

