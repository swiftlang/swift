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

/// A dummy value to be used as the target for `mutationsPtr` in fast enumeration implementations.
fileprivate var _fastEnumerationMutationsTarget: CUnsignedLong = 0
/// A dummy pointer to be used as `mutationsPtr` in fast enumeration implementations.
fileprivate let _fastEnumerationMutationsPtr = UnsafeMutablePointer<CUnsignedLong>(&_fastEnumerationMutationsTarget)

//===----------------------------------------------------------------------===//
// Fast enumeration
//===----------------------------------------------------------------------===//
public struct NSFastEnumerationIterator : IteratorProtocol {
    var enumerable: NSFastEnumeration
    var objects: (Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?, Unmanaged<AnyObject>?) = (nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil)
    var state = NSFastEnumerationState(state: 0, itemsPtr: nil, mutationsPtr: _fastEnumerationMutationsPtr, extra: (0, 0, 0, 0, 0))
    var index = 0
    var count = 0
    var useObjectsBuffer = false
    
    public init(_ enumerable: NSFastEnumeration) {
        self.enumerable = enumerable
    }
    
    public mutating func next() -> Any? {
        if index + 1 > count {
            index = 0
            // ensure NO ivars of self are actually captured
            let enumeratedObject = enumerable
            var localState = state
            var localObjects = objects
            
            (count, useObjectsBuffer) = withUnsafeMutablePointer(to: &localObjects) {
                let buffer = AutoreleasingUnsafeMutablePointer<AnyObject?>($0)
                return withUnsafeMutablePointer(to: &localState) { (statePtr: UnsafeMutablePointer<NSFastEnumerationState>) -> (Int, Bool) in
                    let result = enumeratedObject.countByEnumerating(with: statePtr, objects: buffer, count: 16)
                    if statePtr.pointee.itemsPtr == buffer {
                        // Most cocoa classes will emit their own inner pointer buffers instead of traversing this path. Notable exceptions include NSDictionary and NSSet
                        return (result, true)
                    } else {
                        // this is the common case for things like NSArray
                        return (result, false)
                    }
                }
            }
            
            state = localState // restore the state value
            objects = localObjects // copy the object pointers back to the self storage
            
            if count == 0 { return nil }
        }
        defer { index += 1 }
        if !useObjectsBuffer {
            return state.itemsPtr![index]
        } else {
            switch index {
            case 0: return objects.0!.takeUnretainedValue()
            case 1: return objects.1!.takeUnretainedValue()
            case 2: return objects.2!.takeUnretainedValue()
            case 3: return objects.3!.takeUnretainedValue()
            case 4: return objects.4!.takeUnretainedValue()
            case 5: return objects.5!.takeUnretainedValue()
            case 6: return objects.6!.takeUnretainedValue()
            case 7: return objects.7!.takeUnretainedValue()
            case 8: return objects.8!.takeUnretainedValue()
            case 9: return objects.9!.takeUnretainedValue()
            case 10: return objects.10!.takeUnretainedValue()
            case 11: return objects.11!.takeUnretainedValue()
            case 12: return objects.12!.takeUnretainedValue()
            case 13: return objects.13!.takeUnretainedValue()
            case 14: return objects.14!.takeUnretainedValue()
            case 15: return objects.15!.takeUnretainedValue()
            default: fatalError("Access beyond storage buffer")
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
