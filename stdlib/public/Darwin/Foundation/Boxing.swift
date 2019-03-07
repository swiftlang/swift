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

@_exported import Foundation // Clang module

/// A class type which acts as a handle (pointer-to-pointer) to a Foundation reference type which has only a mutable class (e.g., NSURLComponents).
///
/// Note: This assumes that the result of calling copy() is mutable. The documentation says that classes which do not have a mutable/immutable distinction should just adopt NSCopying instead of NSMutableCopying.
internal final class _MutableHandle<MutableType : NSObject>
  where MutableType : NSCopying {
    fileprivate var _pointer : MutableType
    
    init(reference : __shared MutableType) {
        _pointer = reference.copy() as! MutableType
    }
    
    init(adoptingReference reference: MutableType) {
        _pointer = reference
    }
    
    /// Apply a closure to the reference type.
    func map<ReturnType>(_ whatToDo : (MutableType) throws -> ReturnType) rethrows -> ReturnType {
        return try whatToDo(_pointer)
    }
    
    func _copiedReference() -> MutableType {
        return _pointer.copy() as! MutableType
    }
    
    func _uncopiedReference() -> MutableType {
        return _pointer
    }
}

/// Describes common operations for Foundation struct types that are bridged to a mutable object (e.g. NSURLComponents).
internal protocol _MutableBoxing : ReferenceConvertible {
    var _handle : _MutableHandle<ReferenceType> { get set }
    
    /// Apply a mutating closure to the reference type, regardless if it is mutable or immutable.
    ///
    /// This function performs the correct copy-on-write check for efficient mutation.
    mutating func _applyMutation<ReturnType>(_ whatToDo : (ReferenceType) -> ReturnType) -> ReturnType
}

extension _MutableBoxing {
    @inline(__always)
    mutating func _applyMutation<ReturnType>(_ whatToDo : (ReferenceType) -> ReturnType) -> ReturnType {
        // Only create a new box if we are not uniquely referenced
        if !isKnownUniquelyReferenced(&_handle) {
            let ref = _handle._pointer
            _handle = _MutableHandle(reference: ref)
        }
        return whatToDo(_handle._pointer)
    }
}
