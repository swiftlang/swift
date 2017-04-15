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


//===----------------------------------------------------------------------===//
// Fast enumeration
//===----------------------------------------------------------------------===//
public struct NSFastEnumerationIterator : IteratorProtocol {
    var enumerable: NSFastEnumeration
    var objects: (Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?) = (nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil)
    var state = NSFastEnumerationState(state: 0, itemsPtr: nil, mutationsPtr: _fastEnumerationStorageMutationsPtr, extra: (0, 0, 0, 0, 0))
    var index = 0
    var count = 0
    var useObjectsBuffer = false
    
    public init(_ enumerable: NSFastEnumeration) {
        self.enumerable = enumerable
    }
    
    public mutating func next() -> Any? {
        if index + 1 > count {
            index = 0
            count = withUnsafeMutablePointer(to: &objects) {
                let buffer = AutoreleasingUnsafeMutablePointer<AnyObject?>($0)
                let result = enumerable.countByEnumerating(with: &state, objects: buffer, count: 16)
                if state.itemsPtr == buffer {
                    // Most cocoa classes will emit their own inner pointer buffers instead of traversing this path.
                    useObjectsBuffer = true
                } else {
                    // this is the common case
                    useObjectsBuffer = false
                }
                return result
            }
            if count == 0 { return nil }
        }
        defer { index += 1 }
        if !useObjectsBuffer {
            return state.itemsPtr![index]
        } else {
            return withUnsafePointer(to: &objects) {
                let ptr = UnsafeRawPointer($0).assumingMemoryBound(to: Optional<Unmanaged<AnyObject>>.self)
                return ptr.advanced(by: index).pointee?.takeUnretainedValue()
            }
        }
    }
}

extension NSEnumerator : Sequence {
    /// Return an *iterator* over the *enumerator*.
    ///
    /// - Complexity: O(1).
    public func makeIterator() -> NSFastEnumerationIterator {
        return NSFastEnumerationIterator(self)
    }
}
