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
    
    init(reference : MutableType) {
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

internal enum _MutableUnmanagedWrapper<ImmutableType : NSObject, MutableType : NSObject>
  where MutableType : NSMutableCopying{
    case Immutable(Unmanaged<ImmutableType>)
    case Mutable(Unmanaged<MutableType>)
}

internal protocol _SwiftNativeFoundationType : class {
    associatedtype ImmutableType : NSObject
    associatedtype MutableType : NSObject, NSMutableCopying
    var __wrapped : _MutableUnmanagedWrapper<ImmutableType, MutableType> { get }
    
    init(unmanagedImmutableObject: Unmanaged<ImmutableType>)
    init(unmanagedMutableObject: Unmanaged<MutableType>)
    
    func mutableCopy(with zone : NSZone?) -> Any
    
    var hashValue: Int { get }
    var description: String { get }
    var debugDescription: String { get }
    
    func releaseWrappedObject()
}

extension _SwiftNativeFoundationType {
    
    @inline(__always)
    func _mapUnmanaged<ReturnType>(_ whatToDo : (ImmutableType) throws -> ReturnType) rethrows -> ReturnType {
        defer { _fixLifetime(self) }

        switch __wrapped {
        case .Immutable(let i):
            return try i._withUnsafeGuaranteedRef {
                _onFastPath()
                return try whatToDo($0)
            }
        case .Mutable(let m):
            return try m._withUnsafeGuaranteedRef {
                _onFastPath()
                return try whatToDo(_unsafeReferenceCast($0, to: ImmutableType.self))
            }
        }
    }

    func releaseWrappedObject() {
        switch __wrapped {
        case .Immutable(let i):
            i.release()
        case .Mutable(let m):
            m.release()
        }
    }
    
    func mutableCopy(with zone : NSZone?) -> Any {
        return _mapUnmanaged { $0.mutableCopy() }
    }
    
    var hashValue: Int {
        return _mapUnmanaged { return $0.hashValue }
    }
    
    var description: String {
        return _mapUnmanaged { return $0.description }
    }
    
    var debugDescription: String {
        return _mapUnmanaged { return $0.debugDescription }
    }
    
    func isEqual(_ other: AnyObject) -> Bool {
        return _mapUnmanaged { return $0.isEqual(other) }
    }
}

internal protocol _MutablePairBoxing {
    associatedtype WrappedSwiftNSType : _SwiftNativeFoundationType
    var _wrapped :  WrappedSwiftNSType { get set }
}

extension _MutablePairBoxing {
    @inline(__always)
    func _mapUnmanaged<ReturnType>(_ whatToDo : (WrappedSwiftNSType.ImmutableType) throws -> ReturnType) rethrows -> ReturnType {
        // We are using Unmanaged. Make sure that the owning container class
        // 'self' is guaranteed to be alive by extending the lifetime of 'self'
        // to the end of the scope of this function.
        // Note: At the time of this writing using withExtendedLifetime here
        // instead of _fixLifetime causes different ARC pair matching behavior
        // foiling optimization. This is why we explicitly use _fixLifetime here
        // instead.
        defer { _fixLifetime(self) }

        let unmanagedHandle = Unmanaged.passUnretained(_wrapped)
        let wrapper = unmanagedHandle._withUnsafeGuaranteedRef { $0.__wrapped }
        switch wrapper {
        case .Immutable(let i):
            return try i._withUnsafeGuaranteedRef {
                return try whatToDo($0)
            }
        case .Mutable(let m):
            return try m._withUnsafeGuaranteedRef {
                return try whatToDo(_unsafeReferenceCast($0, to: WrappedSwiftNSType.ImmutableType.self))
            }
        }
    }

    @inline(__always)
    mutating func _applyUnmanagedMutation<ReturnType>(_ whatToDo : (WrappedSwiftNSType.MutableType) throws -> ReturnType) rethrows -> ReturnType {
        // We are using Unmanaged. Make sure that the owning container class
        // 'self' is guaranteed to be alive by extending the lifetime of 'self'
        // to the end of the scope of this function.
        // Note: At the time of this writing using withExtendedLifetime here
        // instead of _fixLifetime causes different ARC pair matching behavior
        // foiling optimization. This is why we explicitly use _fixLifetime here
        // instead.
        defer { _fixLifetime(self) }

        var unique = true
        let _unmanagedHandle = Unmanaged.passUnretained(_wrapped)
        let wrapper = _unmanagedHandle._withUnsafeGuaranteedRef { $0.__wrapped }

        // This check is done twice because: <rdar://problem/24939065> Value kept live for too long causing uniqueness check to fail
        switch wrapper {
        case .Immutable(_):
            break
        case .Mutable(_):
            unique = isKnownUniquelyReferenced(&_wrapped)
        }

        switch wrapper {
        case .Immutable(let i):
            // We need to become mutable; by creating a new instance we also become unique
            let copy = Unmanaged.passRetained(i._withUnsafeGuaranteedRef {
                return _unsafeReferenceCast($0.mutableCopy(), to: WrappedSwiftNSType.MutableType.self) }
            )

            // Be sure to set the var before calling out; otherwise references to the struct in the closure may be looking at the old value
            _wrapped = WrappedSwiftNSType(unmanagedMutableObject: copy)
            return try copy._withUnsafeGuaranteedRef {
                _onFastPath()
                return try whatToDo($0)
            }
        case .Mutable(let m):
            // Only create a new box if we are not uniquely referenced
            if !unique {
                let copy = Unmanaged.passRetained(m._withUnsafeGuaranteedRef {
                    return _unsafeReferenceCast($0.mutableCopy(), to: WrappedSwiftNSType.MutableType.self)
                    })
                _wrapped = WrappedSwiftNSType(unmanagedMutableObject: copy)
                return try copy._withUnsafeGuaranteedRef {
                    _onFastPath()
                    return try whatToDo($0)
                }
            } else {
                return try m._withUnsafeGuaranteedRef {
                    _onFastPath()
                    return try whatToDo($0)
                }
            }
        }
    }
}
