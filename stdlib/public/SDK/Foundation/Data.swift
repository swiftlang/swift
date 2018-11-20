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

#if DEPLOYMENT_RUNTIME_SWIFT

#if os(macOS) || os(iOS)
import Darwin
#elseif os(Linux)
import Glibc
#endif

import CoreFoundation

internal func __NSDataInvokeDeallocatorUnmap(_ mem: UnsafeMutableRawPointer, _ length: Int) {
    munmap(mem, length)
}

internal func __NSDataInvokeDeallocatorFree(_ mem: UnsafeMutableRawPointer, _ length: Int) {
    free(mem)
}

internal func __NSDataIsCompact(_ data: NSData) -> Bool {
    return data._isCompact()
}

#else

@_exported import Foundation // Clang module
import _SwiftFoundationOverlayShims
import _SwiftCoreFoundationOverlayShims

internal func __NSDataIsCompact(_ data: NSData) -> Bool {
    if #available(OSX 10.10, iOS 8.0, tvOS 9.0, watchOS 2.0, *) {
        return data._isCompact()
    } else {
        var compact = true
        let len = data.length
        data.enumerateBytes { (_, byteRange, stop) in
            if byteRange.length != len {
                compact = false
            }
            stop.pointee = true
        }
        return compact
    }
}

#endif

@usableFromInline
internal final class _DataStorage {
    @usableFromInline
    enum Backing {
        // A mirror of the Objective-C implementation that is suitable to inline in Swift
        case swift
        
        // these two storage points for immutable and mutable data are reserved for references that are returned by "known"
        // cases from Foundation in which implement the backing of struct Data, these have signed up for the concept that
        // the backing bytes/mutableBytes pointer does not change per call (unless mutated) as well as the length is ok
        // to be cached, this means that as long as the backing reference is retained no further objc_msgSends need to be
        // dynamically dispatched out to the reference.
        case immutable(NSData) // This will most often (perhaps always) be NSConcreteData
        case mutable(NSMutableData) // This will often (perhaps always) be NSConcreteMutableData
        
        // These are reserved for foreign sources where neither Swift nor Foundation are fully certain whom they belong
        // to from an object inheritance standpoint, this means that all bets are off and the values of bytes, mutableBytes,
        // and length cannot be cached. This also means that all methods are expected to dynamically dispatch out to the
        // backing reference.
        case customReference(NSData) // tracks data references that are only known to be immutable
        case customMutableReference(NSMutableData) // tracks data references that are known to be mutable
    }
    
    static let maxSize = Int.max >> 1
    static let vmOpsThreshold = NSPageSize() * 4
    
    static func allocate(_ size: Int, _ clear: Bool) -> UnsafeMutableRawPointer? {
        if clear {
            return calloc(1, size)
        } else {
            return malloc(size)
        }
    }
    
    @usableFromInline
    static func move(_ dest_: UnsafeMutableRawPointer, _ source_: UnsafeRawPointer?, _ num_: Int) {
        var dest = dest_
        var source = source_
        var num = num_
        if _DataStorage.vmOpsThreshold <= num && ((unsafeBitCast(source, to: Int.self) | Int(bitPattern: dest)) & (NSPageSize() - 1)) == 0 {
            let pages = NSRoundDownToMultipleOfPageSize(num)
            NSCopyMemoryPages(source!, dest, pages)
            source = source!.advanced(by: pages)
            dest = dest.advanced(by: pages)
            num -= pages
        }
        if num > 0 {
            memmove(dest, source!, num)
        }
    }
    
    static func shouldAllocateCleared(_ size: Int) -> Bool {
        return (size > (128 * 1024))
    }
    
    @usableFromInline
    var _bytes: UnsafeMutableRawPointer?
    @usableFromInline
    var _length: Int
    @usableFromInline
    var _capacity: Int
    @usableFromInline
    var _needToZero: Bool
    @usableFromInline
    var _deallocator: ((UnsafeMutableRawPointer, Int) -> Void)?
    @usableFromInline
    var _backing: Backing = .swift
    @usableFromInline
    var _offset: Int
    
    @usableFromInline
    var bytes: UnsafeRawPointer? {
        @inlinable
        get {
            switch _backing {
            case .swift:
                return UnsafeRawPointer(_bytes)?.advanced(by: -_offset)
            case .immutable:
                return UnsafeRawPointer(_bytes)?.advanced(by: -_offset)
            case .mutable:
                return UnsafeRawPointer(_bytes)?.advanced(by: -_offset)
            case .customReference(let d):
                return d.bytes.advanced(by: -_offset)
            case .customMutableReference(let d):
                return d.bytes.advanced(by: -_offset)
            @unknown default:
                fatalError("Unknown Data backing type")
            }
        }
    }

    @usableFromInline
    @discardableResult
    func withUnsafeBytes<Result>(in range: Range<Int>, apply: (UnsafeRawBufferPointer) throws -> Result) rethrows -> Result {
        switch _backing {
        case .swift: fallthrough
        case .immutable: fallthrough
        case .mutable:
            return try apply(UnsafeRawBufferPointer(start: _bytes?.advanced(by: range.lowerBound - _offset), count: Swift.min(range.count, _length)))
        case .customReference(let d):
            if __NSDataIsCompact(d) {
                let len = d.length
                guard len > 0 else {
                    return try apply(UnsafeRawBufferPointer(start: nil, count: 0))
                }
                return try apply(UnsafeRawBufferPointer(start: d.bytes.advanced(by: range.lowerBound - _offset), count: Swift.min(range.count, len)))
            } else {
                var buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: range.count, alignment: MemoryLayout<UInt>.alignment)
                defer { buffer.deallocate() }

                let sliceRange = NSRange(location: range.lowerBound - _offset, length: range.count)
                var enumerated = 0
                d.enumerateBytes { (ptr, byteRange, stop) in
                    if byteRange.upperBound - _offset < range.lowerBound {
                        // before the range that we are looking for...
                    } else if byteRange.lowerBound - _offset > range.upperBound {
                        stop.pointee = true // we are past the range in question so we need to stop
                    } else {
                        // the byteRange somehow intersects the range in question that we are looking for...
                        let lower = Swift.max(byteRange.lowerBound - _offset, range.lowerBound)
                        let upper = Swift.min(byteRange.upperBound - _offset, range.upperBound)

                        let len = upper - lower
                        memcpy(buffer.baseAddress!.advanced(by: enumerated), ptr.advanced(by: lower - (byteRange.lowerBound - _offset)), len)
                        enumerated += len

                        if upper == range.upperBound {
                            stop.pointee = true
                        }
                    }
                }
                return try apply(UnsafeRawBufferPointer(buffer))
            }
        case .customMutableReference(let d):
            if __NSDataIsCompact(d) {
                let len = d.length
                guard len > 0 else {
                    return try apply(UnsafeRawBufferPointer(start: nil, count: 0))
                }
                return try apply(UnsafeRawBufferPointer(start: d.bytes.advanced(by: range.lowerBound - _offset), count: Swift.min(range.count, len)))
            } else {
                var buffer = UnsafeMutableRawBufferPointer.allocate(byteCount: range.count, alignment: MemoryLayout<UInt>.alignment)
                defer { buffer.deallocate() }

                var enumerated = 0
                d.enumerateBytes { (ptr, byteRange, stop) in
                    if byteRange.upperBound - _offset < range.lowerBound {
                        // before the range that we are looking for...
                    } else if byteRange.lowerBound - _offset > range.upperBound {
                        stop.pointee = true // we are past the range in question so we need to stop
                    } else {
                        // the byteRange somehow intersects the range in question that we are looking for...
                        let lower = Swift.max(byteRange.lowerBound - _offset, range.lowerBound)
                        let upper = Swift.min(byteRange.upperBound - _offset, range.upperBound)

                        let len = upper - lower
                        memcpy(buffer.baseAddress!.advanced(by: enumerated), ptr.advanced(by: lower - (byteRange.lowerBound - _offset)), len)
                        enumerated += len

                        if upper == range.upperBound {
                            stop.pointee = true
                        }
                    }
                }
                return try apply(UnsafeRawBufferPointer(buffer))
            }
        }
    }
    
    @usableFromInline
    @discardableResult
    func withUnsafeMutableBytes<Result>(in range: Range<Int>, apply: (UnsafeMutableRawBufferPointer) throws -> Result) rethrows -> Result {
        switch _backing {
        case .swift: fallthrough
        case .mutable:
            return try apply(UnsafeMutableRawBufferPointer(start: _bytes!.advanced(by:range.lowerBound - _offset), count: Swift.min(range.count, _length)))
        case .customMutableReference(let d):
            let len = d.length
            return try apply(UnsafeMutableRawBufferPointer(start: d.mutableBytes.advanced(by:range.lowerBound - _offset), count: Swift.min(range.count, len)))
        case .immutable(let d):
            let data = d.mutableCopy() as! NSMutableData
            _backing = .mutable(data)
            _bytes = data.mutableBytes
            return try apply(UnsafeMutableRawBufferPointer(start: _bytes!.advanced(by:range.lowerBound - _offset), count: Swift.min(range.count, _length)))
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            _backing = .customMutableReference(data)
            let len = data.length
            return try apply(UnsafeMutableRawBufferPointer(start: data.mutableBytes.advanced(by:range.lowerBound - _offset), count: Swift.min(range.count, len)))
        }
    }

    var mutableBytes: UnsafeMutableRawPointer? {
        @inlinable
        get {
            switch _backing {
            case .swift:
                return _bytes?.advanced(by: -_offset)
            case .immutable(let d):
                let data = d.mutableCopy() as! NSMutableData
                data.length = length
                _backing = .mutable(data)
                _bytes = data.mutableBytes
                return _bytes?.advanced(by: -_offset)
            case .mutable:
                return _bytes?.advanced(by: -_offset)
            case .customReference(let d):
                let data = d.mutableCopy() as! NSMutableData
                data.length = length
                _backing = .customMutableReference(data)
                return data.mutableBytes.advanced(by: -_offset)
            case .customMutableReference(let d):
                return d.mutableBytes.advanced(by: -_offset)
            @unknown default:
                fatalError("Unknown Data backing type")
            }
        }
    }
    
    
    @usableFromInline
    var length: Int {
        @inlinable
        get {
            switch _backing {
            case .swift:
                return _length
            case .immutable:
                return _length
            case .mutable:
                return _length
            case .customReference(let d):
                return d.length
            case .customMutableReference(let d):
                return d.length
            @unknown default:
                fatalError("Unknown Data backing type")
            }
        }
        @inlinable
        set {
            setLength(newValue)
        }
    }
    
    
    func _freeBytes() {
        if let bytes = _bytes {
            if let dealloc = _deallocator {
                dealloc(bytes, length)
            } else {
                free(bytes)
            }
        }
    }
    
    func enumerateBytes(in range: Range<Int>, _ block: (_ buffer: UnsafeBufferPointer<UInt8>, _ byteIndex: Data.Index, _ stop: inout Bool) -> Void) {
        var stopv: Bool = false
        var data: NSData
        switch _backing {
        case .swift: fallthrough
        case .immutable: fallthrough
        case .mutable:
            block(UnsafeBufferPointer<UInt8>(start: _bytes?.advanced(by: range.lowerBound - _offset).assumingMemoryBound(to: UInt8.self), count: Swift.min(range.count, _length)), 0, &stopv)
            return
        case .customReference(let d):
            data = d
            break
        case .customMutableReference(let d):
            data = d
            break
        }
        data.enumerateBytes { (ptr, region, stop) in
            // any region that is not in the range should be skipped
            guard range.contains(region.lowerBound) || range.contains(region.upperBound) else { return }
            var regionAdjustment = 0
            if region.lowerBound < range.lowerBound {
                regionAdjustment = range.lowerBound - (region.lowerBound - _offset)
            }
            let bytePtr  = ptr.advanced(by: regionAdjustment).assumingMemoryBound(to: UInt8.self)
            let effectiveLength = Swift.min((region.location - _offset) + region.length, range.upperBound) - (region.location - _offset)
            block(UnsafeBufferPointer(start: bytePtr, count: effectiveLength - regionAdjustment), region.location + regionAdjustment - _offset, &stopv)
            if stopv {
                stop.pointee = true
            }
        }
    }
    
    @usableFromInline
    @inline(never)
    func _grow(_ newLength: Int, _ clear: Bool) {
        let cap = _capacity
        var additionalCapacity = (newLength >> (_DataStorage.vmOpsThreshold <= newLength ? 2 : 1))
        if Int.max - additionalCapacity < newLength {
            additionalCapacity = 0
        }
        var newCapacity = Swift.max(cap, newLength + additionalCapacity)
        let origLength = _length
        var allocateCleared = clear && _DataStorage.shouldAllocateCleared(newCapacity)
        var newBytes: UnsafeMutableRawPointer? = nil
        if _bytes == nil {
            newBytes = _DataStorage.allocate(newCapacity, allocateCleared)
            if newBytes == nil {
                /* Try again with minimum length */
                allocateCleared = clear && _DataStorage.shouldAllocateCleared(newLength)
                newBytes = _DataStorage.allocate(newLength, allocateCleared)
            }
        } else {
            let tryCalloc = (origLength == 0 || (newLength / origLength) >= 4)
            if allocateCleared && tryCalloc {
                newBytes = _DataStorage.allocate(newCapacity, true)
                if let newBytes = newBytes {
                    _DataStorage.move(newBytes, _bytes!, origLength)
                    _freeBytes()
                }
            }
            /* Where calloc/memmove/free fails, realloc might succeed */
            if newBytes == nil {
                allocateCleared = false
                if _deallocator != nil {
                    newBytes = _DataStorage.allocate(newCapacity, true)
                    if let newBytes = newBytes {
                        _DataStorage.move(newBytes, _bytes!, origLength)
                        _freeBytes()
                        _deallocator = nil
                    }
                } else {
                    newBytes = realloc(_bytes!, newCapacity)
                }
            }
            
            /* Try again with minimum length */
            if newBytes == nil {
                newCapacity = newLength
                allocateCleared = clear && _DataStorage.shouldAllocateCleared(newCapacity)
                if allocateCleared && tryCalloc {
                    newBytes = _DataStorage.allocate(newCapacity, true)
                    if let newBytes = newBytes {
                        _DataStorage.move(newBytes, _bytes!, origLength)
                        _freeBytes()
                    }
                }
                if newBytes == nil {
                    allocateCleared = false
                    newBytes = realloc(_bytes!, newCapacity)
                }
            }
        }
        
        if newBytes == nil {
            /* Could not allocate bytes */
            // At this point if the allocation cannot occur the process is likely out of memory
            // and Bad-Things™ are going to happen anyhow
            fatalError("unable to allocate memory for length (\(newLength))")
        }
        
        if origLength < newLength && clear && !allocateCleared {
            memset(newBytes!.advanced(by: origLength), 0, newLength - origLength)
        }
        
        /* _length set by caller */
        _bytes = newBytes
        _capacity = newCapacity
        /* Realloc/memset doesn't zero out the entire capacity, so we must be safe and clear next time we grow the length */
        _needToZero = !allocateCleared
    }
    
    @inlinable
    func setLength(_ length: Int) {
        switch _backing {
        case .swift:
            let origLength = _length
            let newLength = length
            if _capacity < newLength || _bytes == nil {
                _grow(newLength, true)
            } else if origLength < newLength && _needToZero {
                memset(_bytes! + origLength, 0, newLength - origLength)
            } else if newLength < origLength {
                _needToZero = true
            }
            _length = newLength
        case .immutable(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.length = length
            _backing = .mutable(data)
            _length = length
            _bytes = data.mutableBytes
        case .mutable(let d):
            d.length = length
            _length = length
            _bytes = d.mutableBytes
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.length = length
            _backing = .customMutableReference(data)
        case .customMutableReference(let d):
            d.length = length
        @unknown default:
            fatalError("Unknown Data backing type")
        }
    }
    
    @inlinable
    func append(_ bytes: UnsafeRawPointer, length: Int) {
        precondition(length >= 0, "Length of appending bytes must not be negative")
        switch _backing {
        case .swift:
            let origLength = _length
            let newLength = origLength + length
            if _capacity < newLength || _bytes == nil {
                _grow(newLength, false)
            }
            _length = newLength
            _DataStorage.move(_bytes!.advanced(by: origLength), bytes, length)
        case .immutable(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.append(bytes, length: length)
            _backing = .mutable(data)
            _length = data.length
            _bytes = data.mutableBytes
        case .mutable(let d):
            d.append(bytes, length: length)
            _length = d.length
            _bytes = d.mutableBytes
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.append(bytes, length: length)
            _backing = .customMutableReference(data)
        case .customMutableReference(let d):
            d.append(bytes, length: length)
        @unknown default:
            fatalError("Unknown Data backing type")
        }
        
    }
    
    // fast-path for appending directly from another data storage
    @inlinable
    func append(_ otherData: _DataStorage, startingAt start: Int, endingAt end: Int) {
        let otherLength = otherData.length
        if otherLength == 0 { return }
        if let bytes = otherData.bytes {
            append(bytes.advanced(by: start), length: end - start)
        }
    }
    
    @inlinable
    func append(_ otherData: Data) {
        otherData.enumerateBytes { (buffer: UnsafeBufferPointer<UInt8>, _, _) in
            append(buffer.baseAddress!, length: buffer.count)
        }
    }
    
    @inlinable
    func increaseLength(by extraLength: Int) {
        if extraLength == 0 { return }
        switch _backing {
        case .swift:
            let origLength = _length
            let newLength = origLength + extraLength
            if _capacity < newLength || _bytes == nil {
                _grow(newLength, true)
            } else if _needToZero {
                memset(_bytes!.advanced(by: origLength), 0, extraLength)
            }
            _length = newLength
        case .immutable(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.increaseLength(by: extraLength)
            _backing = .mutable(data)
            _length += extraLength
            _bytes = data.mutableBytes
        case .mutable(let d):
            d.increaseLength(by: extraLength)
            _length += extraLength
            _bytes = d.mutableBytes
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.increaseLength(by: extraLength)
            _backing = .customReference(data)
        case .customMutableReference(let d):
            d.increaseLength(by: extraLength)
        @unknown default:
            fatalError("Unknown Data backing type")
        }
        
    }

    @usableFromInline
    func get(_ index: Int) -> UInt8 {
        switch _backing {
        case .swift: fallthrough
        case .immutable: fallthrough
        case .mutable:
            return _bytes!.advanced(by: index - _offset).assumingMemoryBound(to: UInt8.self).pointee
        case .customReference(let d):
            if __NSDataIsCompact(d) {
                return d.bytes.advanced(by: index - _offset).assumingMemoryBound(to: UInt8.self).pointee
            } else {
                var byte: UInt8 = 0
                d.enumerateBytes { (ptr, range, stop) in
                    if NSLocationInRange(index, range) {
                        let offset = index - range.location - _offset
                        byte = ptr.advanced(by: offset).assumingMemoryBound(to: UInt8.self).pointee
                        stop.pointee = true
                    }
                }
                return byte
            }
        case .customMutableReference(let d):
            if __NSDataIsCompact(d) {
                return d.bytes.advanced(by: index - _offset).assumingMemoryBound(to: UInt8.self).pointee
            } else {
                var byte: UInt8 = 0
                d.enumerateBytes { (ptr, range, stop) in
                    if NSLocationInRange(index, range) {
                        let offset = index - range.location - _offset
                        byte = ptr.advanced(by: offset).assumingMemoryBound(to: UInt8.self).pointee
                        stop.pointee = true
                    }
                }
                return byte
            }
        }
    }
    
    @inlinable
    func set(_ index: Int, to value: UInt8) {
        switch _backing {
        case .swift:
            fallthrough
        case .mutable:
            _bytes!.advanced(by: index - _offset).assumingMemoryBound(to: UInt8.self).pointee = value
        default:
            var theByte = value
            let range = NSRange(location: index, length: 1)
            replaceBytes(in: range, with: &theByte, length: 1)
        }
        
    }
    
    @inlinable
    func replaceBytes(in range: NSRange, with bytes: UnsafeRawPointer?) {
        if range.length == 0 { return }
        switch _backing {
        case .swift:
            if _length < range.location + range.length {
                let newLength = range.location + range.length
                if _capacity < newLength {
                    _grow(newLength, false)
                }
                _length = newLength
            }
            _DataStorage.move(_bytes!.advanced(by: range.location - _offset), bytes!, range.length)
        case .immutable(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.replaceBytes(in: NSRange(location: range.location - _offset, length: range.length), withBytes: bytes!)
            _backing = .mutable(data)
            _length = data.length
            _bytes = data.mutableBytes
        case .mutable(let d):
            d.replaceBytes(in: NSRange(location: range.location - _offset, length: range.length), withBytes: bytes!)
            _length = d.length
            _bytes = d.mutableBytes
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.replaceBytes(in: NSRange(location: range.location - _offset, length: range.length), withBytes: bytes!)
            _backing = .customMutableReference(data)
        case .customMutableReference(let d):
            d.replaceBytes(in: NSRange(location: range.location - _offset, length: range.length), withBytes: bytes!)
        @unknown default:
            fatalError("Unknown Data backing type")
        }
    }
    
    @inlinable
    func replaceBytes(in range_: NSRange, with replacementBytes: UnsafeRawPointer?, length replacementLength: Int) {
        let range = NSRange(location: range_.location - _offset, length: range_.length)
        let currentLength = _length
        let resultingLength = currentLength - range.length + replacementLength
        switch _backing {
        case .swift:
            let shift = resultingLength - currentLength
            var mutableBytes = _bytes
            if resultingLength > currentLength {
                setLength(resultingLength)
                mutableBytes = _bytes!
            }
            /* shift the trailing bytes */
            let start = range.location
            let length = range.length
            if shift != 0 {
                memmove(mutableBytes! + start + replacementLength, mutableBytes! + start + length, currentLength - start - length)
            }
            if replacementLength != 0 {
                if let replacementBytes = replacementBytes {
                    memmove(mutableBytes! + start, replacementBytes, replacementLength)
                } else {
                    memset(mutableBytes! + start, 0, replacementLength)
                }
            }
            
            if resultingLength < currentLength {
                setLength(resultingLength)
            }
        case .immutable(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.replaceBytes(in: range, withBytes: replacementBytes, length: replacementLength)
            _backing = .mutable(data)
            _length = data.length
            _bytes = data.mutableBytes
        case .mutable(let d):
            d.replaceBytes(in: range, withBytes: replacementBytes, length: replacementLength)
            _backing = .mutable(d)
            _length = d.length
            _bytes = d.mutableBytes
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.replaceBytes(in: range, withBytes: replacementBytes, length: replacementLength)
            _backing = .customMutableReference(data)
        case .customMutableReference(let d):
            d.replaceBytes(in: range, withBytes: replacementBytes, length: replacementLength)
        @unknown default:
            fatalError("Unknown Data backing type")
        }
    }
    
    @inlinable
    func resetBytes(in range_: NSRange) {
        let range = NSRange(location: range_.location - _offset, length: range_.length)
        if range.length == 0 { return }
        switch _backing {
        case .swift:
            if _length < range.location + range.length {
                let newLength = range.location + range.length
                if _capacity < newLength {
                    _grow(newLength, false)
                }
                _length = newLength
            }
            memset(_bytes!.advanced(by: range.location), 0, range.length)
        case .immutable(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.resetBytes(in: range)
            _backing = .mutable(data)
            _length = data.length
            _bytes = data.mutableBytes
        case .mutable(let d):
            d.resetBytes(in: range)
            _length = d.length
            _bytes = d.mutableBytes
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.resetBytes(in: range)
            _backing = .customMutableReference(data)
        case .customMutableReference(let d):
            d.resetBytes(in: range)
        @unknown default:
            fatalError("Unknown Data backing type")
        }
        
    }
    
    @usableFromInline
    convenience init() {
        self.init(capacity: 0)
    }
    
    @usableFromInline
    init(length: Int) {
        precondition(length < _DataStorage.maxSize)
        var capacity = (length < 1024 * 1024 * 1024) ? length + (length >> 2) : length
        if _DataStorage.vmOpsThreshold <= capacity {
            capacity = NSRoundUpToMultipleOfPageSize(capacity)
        }
        
        let clear = _DataStorage.shouldAllocateCleared(length)
        _bytes = _DataStorage.allocate(capacity, clear)!
        _capacity = capacity
        _needToZero = !clear
        _length = 0
        _offset = 0
        setLength(length)
    }
    
    @usableFromInline
    init(capacity capacity_: Int) {
        var capacity = capacity_
        precondition(capacity < _DataStorage.maxSize)
        if _DataStorage.vmOpsThreshold <= capacity {
            capacity = NSRoundUpToMultipleOfPageSize(capacity)
        }
        _length = 0
        _bytes = _DataStorage.allocate(capacity, false)!
        _capacity = capacity
        _needToZero = true
        _offset = 0
    }
    
    @usableFromInline
    init(bytes: UnsafeRawPointer?, length: Int) {
        precondition(length < _DataStorage.maxSize)
        _offset = 0
        if length == 0 {
            _capacity = 0
            _length = 0
            _needToZero = false
            _bytes = nil
        } else if _DataStorage.vmOpsThreshold <= length {
            _capacity = length
            _length = length
            _needToZero = true
            _bytes = _DataStorage.allocate(length, false)!
            _DataStorage.move(_bytes!, bytes, length)
        } else {
            var capacity = length
            if _DataStorage.vmOpsThreshold <= capacity {
                capacity = NSRoundUpToMultipleOfPageSize(capacity)
            }
            _length = length
            _bytes = _DataStorage.allocate(capacity, false)!
            _capacity = capacity
            _needToZero = true
            _DataStorage.move(_bytes!, bytes, length)
        }
    }
    
    @usableFromInline
    init(bytes: UnsafeMutableRawPointer?, length: Int, copy: Bool, deallocator: ((UnsafeMutableRawPointer, Int) -> Void)?, offset: Int) {
        precondition(length < _DataStorage.maxSize)
        _offset = offset
        if length == 0 {
            _capacity = 0
            _length = 0
            _needToZero = false
            _bytes = nil
            if let dealloc = deallocator,
               let bytes_ = bytes {
                dealloc(bytes_, length)
            }
        } else if !copy {
            _capacity = length
            _length = length
            _needToZero = false
            _bytes = bytes
            _deallocator = deallocator
        } else if _DataStorage.vmOpsThreshold <= length {
            _capacity = length
            _length = length
            _needToZero = true
            _bytes = _DataStorage.allocate(length, false)!
            _DataStorage.move(_bytes!, bytes, length)
            if let dealloc = deallocator {
                dealloc(bytes!, length)
            }
        } else {
            var capacity = length
            if _DataStorage.vmOpsThreshold <= capacity {
                capacity = NSRoundUpToMultipleOfPageSize(capacity)
            }
            _length = length
            _bytes = _DataStorage.allocate(capacity, false)!
            _capacity = capacity
            _needToZero = true
            _DataStorage.move(_bytes!, bytes, length)
            if let dealloc = deallocator {
                dealloc(bytes!, length)
            }
        }
    }
    
    @usableFromInline
    init(immutableReference: NSData, offset: Int) {
        _offset = offset
        _bytes = UnsafeMutableRawPointer(mutating: immutableReference.bytes)
        _capacity = 0
        _needToZero = false
        _length = immutableReference.length
        _backing = .immutable(immutableReference)
    }
    
    @usableFromInline
    init(mutableReference: NSMutableData, offset: Int) {
        _offset = offset
        _bytes = mutableReference.mutableBytes
        _capacity = 0
        _needToZero = false
        _length = mutableReference.length
        _backing = .mutable(mutableReference)
    }
    
    @usableFromInline
    init(customReference: NSData, offset: Int) {
        _offset = offset
        _bytes = nil
        _capacity = 0
        _needToZero = false
        _length = 0
        _backing = .customReference(customReference)
    }
    
    @usableFromInline
    init(customMutableReference: NSMutableData, offset: Int) {
        _offset = offset
        _bytes = nil
        _capacity = 0
        _needToZero = false
        _length = 0
        _backing = .customMutableReference(customMutableReference)
    }
    
    deinit {
        switch _backing {
        case .swift:
            _freeBytes()
        default:
            break
        }
    }
    
    @inlinable
    func mutableCopy(_ range: Range<Int>) -> _DataStorage {
        switch _backing {
        case .swift:
            return _DataStorage(bytes: _bytes?.advanced(by: range.lowerBound - _offset), length: range.count, copy: true, deallocator: nil, offset: range.lowerBound)
        case .immutable(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return _DataStorage(mutableReference: d.mutableCopy() as! NSMutableData, offset: range.lowerBound)
            } else {
                return _DataStorage(mutableReference: d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC().mutableCopy() as! NSMutableData, offset: range.lowerBound)
            }
        case .mutable(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return _DataStorage(mutableReference: d.mutableCopy() as! NSMutableData, offset: range.lowerBound)
            } else {
                return _DataStorage(mutableReference: d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC().mutableCopy() as! NSMutableData, offset: range.lowerBound)
            }
        case .customReference(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return _DataStorage(mutableReference: d.mutableCopy() as! NSMutableData, offset: range.lowerBound)
            } else {
                return _DataStorage(mutableReference: d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC().mutableCopy() as! NSMutableData, offset: range.lowerBound)
            }
        case .customMutableReference(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return _DataStorage(mutableReference: d.mutableCopy() as! NSMutableData, offset: range.lowerBound)
            } else {
                return _DataStorage(mutableReference: d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC().mutableCopy() as! NSMutableData, offset: range.lowerBound)
            }
        @unknown default:
            fatalError("Unknown Data backing type")
        }
    }
    
    func withInteriorPointerReference<T>(_ range: Range<Int>, _ work: (NSData) throws -> T) rethrows -> T {
        if range.isEmpty {
            return try work(NSData()) // zero length data can be optimized as a singleton
        }
        
        switch _backing {
        case .swift:
            return try work(NSData(bytesNoCopy: _bytes!.advanced(by: range.lowerBound - _offset), length: range.count, freeWhenDone: false))
        case .immutable(let d):
            guard range.lowerBound == 0 && range.upperBound == _length else {
                return try work(NSData(bytesNoCopy: _bytes!.advanced(by: range.lowerBound - _offset), length: range.count, freeWhenDone: false))
            }
            return try work(d)
        case .mutable(let d):
            guard range.lowerBound == 0 && range.upperBound == _length else {
                return try work(NSData(bytesNoCopy: _bytes!.advanced(by: range.lowerBound - _offset), length: range.count, freeWhenDone: false))
            }
            return try work(d)
        case .customReference(let d):
            guard range.lowerBound == 0 && range.upperBound == _length else {
                
                return try work(NSData(bytesNoCopy: UnsafeMutableRawPointer(mutating: d.bytes.advanced(by: range.lowerBound - _offset)), length: range.count, freeWhenDone: false))
            }
            return try work(d)
        case .customMutableReference(let d):
            guard range.lowerBound == 0 && range.upperBound == _length else {
                return try work(NSData(bytesNoCopy: UnsafeMutableRawPointer(mutating: d.bytes.advanced(by: range.lowerBound - _offset)), length: range.count, freeWhenDone: false))
            }
            return try work(d)
        }
    }
    
    func bridgedReference(_ range: Range<Int>) -> NSData {
        if range.isEmpty {
            return NSData() // zero length data can be optimized as a singleton
        }
        
        switch _backing {
        case .swift:
            return __NSSwiftData(backing: self, range: range)
        case .immutable(let d):
            guard range.lowerBound == 0 && range.upperBound == _length else {
                return __NSSwiftData(backing: self, range: range)
            }
            return d
        case .mutable(let d):
            guard range.lowerBound == 0 && range.upperBound == _length else {
                return __NSSwiftData(backing: self, range: range)
            }
            return d
        case .customReference(let d):
            guard range.lowerBound == 0 && range.upperBound == d.length else {
                return __NSSwiftData(backing: self, range: range)
            }
            return d
        case .customMutableReference(let d):
            guard range.lowerBound == 0 && range.upperBound == d.length else {
                return d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC()
            }
            return d.copy() as! NSData
        }
    }
    
    @usableFromInline
    func subdata(in range: Range<Data.Index>) -> Data {
        switch _backing {
        case .customReference(let d):
            return d.subdata(with: NSRange(location: range.lowerBound - _offset, length: range.count))
        case .customMutableReference(let d):
            return d.subdata(with: NSRange(location: range.lowerBound - _offset, length: range.count))
        default:
            return Data(bytes: _bytes!.advanced(by: range.lowerBound - _offset), count: range.count)
        }
    }
}

// NOTE: older runtimes called this _NSSwiftData. The two must
// coexist, so it was renamed. The old name must not be used in the new
// runtime.
internal class __NSSwiftData : NSData {
    var _backing: _DataStorage!
    var _range: Range<Data.Index>!
    
    convenience init(backing: _DataStorage, range: Range<Data.Index>) {
        self.init()
        _backing = backing
        _range = range
    }
    override var length: Int {
        return _range.count
    }
    
    override var bytes: UnsafeRawPointer {
        // NSData's byte pointer methods are not annotated for nullability correctly
        // (but assume non-null by the wrapping macro guards). This placeholder value
        // is to work-around this bug. Any indirection to the underlying bytes of an NSData
        // with a length of zero would have been a programmer error anyhow so the actual
        // return value here is not needed to be an allocated value. This is specifically
        // needed to live like this to be source compatible with Swift3. Beyond that point
        // this API may be subject to correction.
        guard let bytes = _backing.bytes else {
            return UnsafeRawPointer(bitPattern: 0xBAD0)!
        }
        
        return bytes.advanced(by: _range.lowerBound)
    }
    
    override func copy(with zone: NSZone? = nil) -> Any {
        return self
    }
    
    override func mutableCopy(with zone: NSZone? = nil) -> Any {
        return NSMutableData(bytes: bytes, length: length)
    }
    
#if !DEPLOYMENT_RUNTIME_SWIFT
    @objc override
    func _isCompact() -> Bool {
        return true
    }
#endif
    
#if DEPLOYMENT_RUNTIME_SWIFT
    override func _providesConcreteBacking() -> Bool {
        return true
    }
#else
    @objc(_providesConcreteBacking)
    func _providesConcreteBacking() -> Bool {
        return true
    }
#endif
}

public struct Data : ReferenceConvertible, Equatable, Hashable, RandomAccessCollection, MutableCollection, RangeReplaceableCollection {
    public typealias ReferenceType = NSData
    
    public typealias ReadingOptions = NSData.ReadingOptions
    public typealias WritingOptions = NSData.WritingOptions
    public typealias SearchOptions = NSData.SearchOptions
    public typealias Base64EncodingOptions = NSData.Base64EncodingOptions
    public typealias Base64DecodingOptions = NSData.Base64DecodingOptions
    
    public typealias Index = Int
    public typealias Indices = Range<Int>
    
    @usableFromInline internal var _backing : _DataStorage
    @usableFromInline internal var _sliceRange: Range<Index>
    
    
    // A standard or custom deallocator for `Data`.
    ///
    /// When creating a `Data` with the no-copy initializer, you may specify a `Data.Deallocator` to customize the behavior of how the backing store is deallocated.
    public enum Deallocator {
        /// Use a virtual memory deallocator.
#if !DEPLOYMENT_RUNTIME_SWIFT
        case virtualMemory
#endif
        
        /// Use `munmap`.
        case unmap
        
        /// Use `free`.
        case free
        
        /// Do nothing upon deallocation.
        case none
        
        /// A custom deallocator.
        case custom((UnsafeMutableRawPointer, Int) -> Void)
        
        fileprivate var _deallocator : ((UnsafeMutableRawPointer, Int) -> Void) {
#if DEPLOYMENT_RUNTIME_SWIFT
            switch self {
            case .unmap:
                return { __NSDataInvokeDeallocatorUnmap($0, $1) }
            case .free:
                return { __NSDataInvokeDeallocatorFree($0, $1) }
            case .none:
                return { _, _ in }
            case .custom(let b):
                return { (ptr, len) in
                    b(ptr, len)
                }
            }
#else
            switch self {
            case .virtualMemory:
                return { NSDataDeallocatorVM($0, $1) }
            case .unmap:
                return { NSDataDeallocatorUnmap($0, $1) }
            case .free:
                return { NSDataDeallocatorFree($0, $1) }
            case .none:
                return { _, _ in }
            case .custom(let b):
                return { (ptr, len) in
                    b(ptr, len)
                }
            }
#endif
        }
    }
    
    // MARK: -
    // MARK: Init methods
    
    /// Initialize a `Data` with copied memory content.
    ///
    /// - parameter bytes: A pointer to the memory. It will be copied.
    /// - parameter count: The number of bytes to copy.
    public init(bytes: UnsafeRawPointer, count: Int) {
        _backing = _DataStorage(bytes: bytes, length: count)
        _sliceRange = 0..<count
    }
    
    /// Initialize a `Data` with copied memory content.
    ///
    /// - parameter buffer: A buffer pointer to copy. The size is calculated from `SourceType` and `buffer.count`.
    public init<SourceType>(buffer: UnsafeBufferPointer<SourceType>) {
        let count = MemoryLayout<SourceType>.stride * buffer.count
        _backing = _DataStorage(bytes: buffer.baseAddress, length: count)
        _sliceRange = 0..<count
    }
    
    /// Initialize a `Data` with copied memory content.
    ///
    /// - parameter buffer: A buffer pointer to copy. The size is calculated from `SourceType` and `buffer.count`.
    public init<SourceType>(buffer: UnsafeMutableBufferPointer<SourceType>) {
        let count = MemoryLayout<SourceType>.stride * buffer.count
        _backing = _DataStorage(bytes: buffer.baseAddress, length: count)
        _sliceRange = 0..<count
    }
    
    /// Initialize a `Data` with a repeating byte pattern
    ///
    /// - parameter repeatedValue: A byte to initialize the pattern
    /// - parameter count: The number of bytes the data initially contains initialized to the repeatedValue
    public init(repeating repeatedValue: UInt8, count: Int) {
        self.init(count: count)
        withUnsafeMutableBytes { (bytes: UnsafeMutablePointer<UInt8>) -> Void in
            memset(bytes, Int32(repeatedValue), count)
        }
    }
    
    /// Initialize a `Data` with the specified size.
    ///
    /// This initializer doesn't necessarily allocate the requested memory right away. `Data` allocates additional memory as needed, so `capacity` simply establishes the initial capacity. When it does allocate the initial memory, though, it allocates the specified amount.
    ///
    /// This method sets the `count` of the data to 0.
    ///
    /// If the capacity specified in `capacity` is greater than four memory pages in size, this may round the amount of requested memory up to the nearest full page.
    ///
    /// - parameter capacity: The size of the data.
    public init(capacity: Int) {
        _backing = _DataStorage(capacity: capacity)
        _sliceRange = 0..<0
    }
    
    /// Initialize a `Data` with the specified count of zeroed bytes.
    ///
    /// - parameter count: The number of bytes the data initially contains.
    public init(count: Int) {
        _backing = _DataStorage(length: count)
        _sliceRange = 0..<count
    }
    
    /// Initialize an empty `Data`.
    public init() {
        _backing = _DataStorage(length: 0)
        _sliceRange = 0..<0
    }
    
    
    /// Initialize a `Data` without copying the bytes.
    ///
    /// If the result is mutated and is not a unique reference, then the `Data` will still follow copy-on-write semantics. In this case, the copy will use its own deallocator. Therefore, it is usually best to only use this initializer when you either enforce immutability with `let` or ensure that no other references to the underlying data are formed.
    /// - parameter bytes: A pointer to the bytes.
    /// - parameter count: The size of the bytes.
    /// - parameter deallocator: Specifies the mechanism to free the indicated buffer, or `.none`.
    public init(bytesNoCopy bytes: UnsafeMutableRawPointer, count: Int, deallocator: Deallocator) {
        let whichDeallocator = deallocator._deallocator
        _backing = _DataStorage(bytes: bytes, length: count, copy: false, deallocator: whichDeallocator, offset: 0)
        _sliceRange = 0..<count
    }
    
    /// Initialize a `Data` with the contents of a `URL`.
    ///
    /// - parameter url: The `URL` to read.
    /// - parameter options: Options for the read operation. Default value is `[]`.
    /// - throws: An error in the Cocoa domain, if `url` cannot be read.
    public init(contentsOf url: __shared URL, options: Data.ReadingOptions = []) throws {
        let d = try NSData(contentsOf: url, options: ReadingOptions(rawValue: options.rawValue))
        _backing = _DataStorage(immutableReference: d, offset: 0)
        _sliceRange = 0..<d.length
    }
    
    /// Initialize a `Data` from a Base-64 encoded String using the given options.
    ///
    /// Returns nil when the input is not recognized as valid Base-64.
    /// - parameter base64String: The string to parse.
    /// - parameter options: Encoding options. Default value is `[]`.
    public init?(base64Encoded base64String: __shared String, options: Data.Base64DecodingOptions = []) {
        if let d = NSData(base64Encoded: base64String, options: Base64DecodingOptions(rawValue: options.rawValue)) {
            _backing = _DataStorage(immutableReference: d, offset: 0)
            _sliceRange = 0..<d.length
        } else {
            return nil
        }
    }
    
    /// Initialize a `Data` from a Base-64, UTF-8 encoded `Data`.
    ///
    /// Returns nil when the input is not recognized as valid Base-64.
    ///
    /// - parameter base64Data: Base-64, UTF-8 encoded input data.
    /// - parameter options: Decoding options. Default value is `[]`.
    public init?(base64Encoded base64Data: __shared Data, options: Data.Base64DecodingOptions = []) {
        if let d = NSData(base64Encoded: base64Data, options: Base64DecodingOptions(rawValue: options.rawValue)) {
            _backing = _DataStorage(immutableReference: d, offset: 0)
            _sliceRange = 0..<d.length
        } else {
            return nil
        }
    }
    
    /// Initialize a `Data` by adopting a reference type.
    ///
    /// You can use this initializer to create a `struct Data` that wraps a `class NSData`. `struct Data` will use the `class NSData` for all operations. Other initializers (including casting using `as Data`) may choose to hold a reference or not, based on a what is the most efficient representation.
    ///
    /// If the resulting value is mutated, then `Data` will invoke the `mutableCopy()` function on the reference to copy the contents. You may customize the behavior of that function if you wish to return a specialized mutable subclass.
    ///
    /// - parameter reference: The instance of `NSData` that you wish to wrap. This instance will be copied by `struct Data`.
    public init(referencing reference: __shared NSData) {
#if DEPLOYMENT_RUNTIME_SWIFT
        let providesConcreteBacking = reference._providesConcreteBacking()
#else
        let providesConcreteBacking = (reference as AnyObject)._providesConcreteBacking?() ?? false
#endif
        if providesConcreteBacking {
            _backing = _DataStorage(immutableReference: reference.copy() as! NSData, offset: 0)
            _sliceRange = 0..<reference.length
        } else {
            _backing = _DataStorage(customReference: reference.copy() as! NSData, offset: 0)
            _sliceRange = 0..<reference.length
        }
        
    }
    
    // slightly faster paths for common sequences
    @inlinable
    public init<S: Sequence>(_ elements: S) where S.Iterator.Element == UInt8 {
        let backing = _DataStorage(capacity: Swift.max(elements.underestimatedCount, 1))
        var (iter, endIndex) = elements._copyContents(initializing: UnsafeMutableBufferPointer(start: backing._bytes?.bindMemory(to: UInt8.self, capacity: backing._capacity), count: backing._capacity))
        backing._length = endIndex
        while var element = iter.next() {
            backing.append(&element, length: 1)
        }
        self.init(backing: backing, range: 0..<backing._length)
    }
    
    @available(swift, introduced: 4.2)
    @inlinable
    public init<S: Sequence>(bytes elements: S) where S.Iterator.Element == UInt8 {
        self.init(elements)
    }

    @available(swift, obsoleted: 4.2)
    public init(bytes: Array<UInt8>) {
       self.init(bytes)
    }
    
    @available(swift, obsoleted: 4.2)
    public init(bytes: ArraySlice<UInt8>) {
       self.init(bytes)
    }

    @usableFromInline
    internal init(backing: _DataStorage, range: Range<Index>) {
        _backing = backing
        _sliceRange = range
    }
    
    @usableFromInline
    internal func _validateIndex(_ index: Int, message: String? = nil) {
        precondition(_sliceRange.contains(index), message ?? "Index \(index) is out of bounds of range \(_sliceRange)")
    }
    
    @usableFromInline
    internal func _validateRange<R: RangeExpression>(_ range: R) where R.Bound == Int {
        let lower = R.Bound(_sliceRange.lowerBound)
        let upper = R.Bound(_sliceRange.upperBound)
        let r = range.relative(to: lower..<upper)
        precondition(r.lowerBound >= _sliceRange.lowerBound && r.lowerBound <= _sliceRange.upperBound, "Range \(r) is out of bounds of range \(_sliceRange)")
        precondition(r.upperBound >= _sliceRange.lowerBound && r.upperBound <= _sliceRange.upperBound, "Range \(r) is out of bounds of range \(_sliceRange)")
    }
    
    // -----------------------------------
    // MARK: - Properties and Functions
    
    /// The number of bytes in the data.
    
    public var count: Int {
        get {
            return _sliceRange.count
        }
        set {
            precondition(count >= 0, "count must not be negative")
            if !isKnownUniquelyReferenced(&_backing) {
                _backing = _backing.mutableCopy(_sliceRange)
            }
            _backing.length = newValue
            _sliceRange = _sliceRange.lowerBound..<(_sliceRange.lowerBound + newValue)
        }
    }
    
    /// Access the bytes in the data.
    ///
    /// - warning: The byte pointer argument should not be stored and used outside of the lifetime of the call to the closure.
    public func withUnsafeBytes<ResultType, ContentType>(_ body: (UnsafePointer<ContentType>) throws -> ResultType) rethrows -> ResultType {
        return try _backing.withUnsafeBytes(in: _sliceRange) {
            return try body($0.baseAddress?.assumingMemoryBound(to: ContentType.self) ?? UnsafePointer<ContentType>(bitPattern: 0xBAD0)!)
        }
    }
    
    
    /// Mutate the bytes in the data.
    ///
    /// This function assumes that you are mutating the contents.
    /// - warning: The byte pointer argument should not be stored and used outside of the lifetime of the call to the closure.
    public mutating func withUnsafeMutableBytes<ResultType, ContentType>(_ body: (UnsafeMutablePointer<ContentType>) throws -> ResultType) rethrows -> ResultType {
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        return try _backing.withUnsafeMutableBytes(in: _sliceRange) {
            return try body($0.baseAddress?.assumingMemoryBound(to: ContentType.self) ?? UnsafeMutablePointer<ContentType>(bitPattern: 0xBAD0)!)
        }
    }
    
    // MARK: -
    // MARK: Copy Bytes
    
    /// Copy the contents of the data to a pointer.
    ///
    /// - parameter pointer: A pointer to the buffer you wish to copy the bytes into.
    /// - parameter count: The number of bytes to copy.
    /// - warning: This method does not verify that the contents at pointer have enough space to hold `count` bytes.
    public func copyBytes(to pointer: UnsafeMutablePointer<UInt8>, count: Int) {
        precondition(count >= 0, "count of bytes to copy must not be negative")
        if count == 0 { return }
        _backing.withUnsafeBytes(in: _sliceRange) {
            memcpy(UnsafeMutableRawPointer(pointer), $0.baseAddress!, Swift.min(count, $0.count))
        }
    }
    
    private func _copyBytesHelper(to pointer: UnsafeMutableRawPointer, from range: NSRange) {
        if range.length == 0 { return }
        _backing.withUnsafeBytes(in: range.lowerBound..<range.upperBound) {
            memcpy(UnsafeMutableRawPointer(pointer), $0.baseAddress!, Swift.min(range.length, $0.count))
        }
    }
    
    /// Copy a subset of the contents of the data to a pointer.
    ///
    /// - parameter pointer: A pointer to the buffer you wish to copy the bytes into.
    /// - parameter range: The range in the `Data` to copy.
    /// - warning: This method does not verify that the contents at pointer have enough space to hold the required number of bytes.
    public func copyBytes(to pointer: UnsafeMutablePointer<UInt8>, from range: Range<Index>) {
        _copyBytesHelper(to: pointer, from: NSRange(range))
    }
    
    // Copy the contents of the data into a buffer.
    ///
    /// This function copies the bytes in `range` from the data into the buffer. If the count of the `range` is greater than `MemoryLayout<DestinationType>.stride * buffer.count` then the first N bytes will be copied into the buffer.
    /// - precondition: The range must be within the bounds of the data. Otherwise `fatalError` is called.
    /// - parameter buffer: A buffer to copy the data into.
    /// - parameter range: A range in the data to copy into the buffer. If the range is empty, this function will return 0 without copying anything. If the range is nil, as much data as will fit into `buffer` is copied.
    /// - returns: Number of bytes copied into the destination buffer.
    public func copyBytes<DestinationType>(to buffer: UnsafeMutableBufferPointer<DestinationType>, from range: Range<Index>? = nil) -> Int {
        let cnt = count
        guard cnt > 0 else { return 0 }
        
        let copyRange : Range<Index>
        if let r = range {
            guard !r.isEmpty else { return 0 }
            copyRange = r.lowerBound..<(r.lowerBound + Swift.min(buffer.count * MemoryLayout<DestinationType>.stride, r.count))
        } else {
            copyRange = 0..<Swift.min(buffer.count * MemoryLayout<DestinationType>.stride, cnt)
        }
        _validateRange(copyRange)
        
        guard !copyRange.isEmpty else { return 0 }
        
        let nsRange = NSRange(location: copyRange.lowerBound, length: copyRange.upperBound - copyRange.lowerBound)
        _copyBytesHelper(to: buffer.baseAddress!, from: nsRange)
        return copyRange.count
    }
    
    // MARK: -
#if !DEPLOYMENT_RUNTIME_SWIFT
    private func _shouldUseNonAtomicWriteReimplementation(options: Data.WritingOptions = []) -> Bool {
    
        // Avoid a crash that happens on OS X 10.11.x and iOS 9.x or before when writing a bridged Data non-atomically with Foundation's standard write() implementation.
        if !options.contains(.atomic) {
            #if os(macOS)
                return NSFoundationVersionNumber <= Double(NSFoundationVersionNumber10_11_Max)
            #else
                return NSFoundationVersionNumber <= Double(NSFoundationVersionNumber_iOS_9_x_Max)
            #endif
        } else {
            return false
        }
    }
#endif
    
    /// Write the contents of the `Data` to a location.
    ///
    /// - parameter url: The location to write the data into.
    /// - parameter options: Options for writing the data. Default value is `[]`.
    /// - throws: An error in the Cocoa domain, if there is an error writing to the `URL`.
    public func write(to url: URL, options: Data.WritingOptions = []) throws {
        try _backing.withInteriorPointerReference(_sliceRange) {
#if DEPLOYMENT_RUNTIME_SWIFT
            try $0.write(to: url, options: WritingOptions(rawValue: options.rawValue))
#else
            if _shouldUseNonAtomicWriteReimplementation(options: options) {
                var error: NSError? = nil
                guard __NSDataWriteToURL($0, url, options, &error) else { throw error! }
            } else {
                try $0.write(to: url, options: options)
            }
#endif
        }
    }
    
    // MARK: -
    
    /// Find the given `Data` in the content of this `Data`.
    ///
    /// - parameter dataToFind: The data to be searched for.
    /// - parameter options: Options for the search. Default value is `[]`.
    /// - parameter range: The range of this data in which to perform the search. Default value is `nil`, which means the entire content of this data.
    /// - returns: A `Range` specifying the location of the found data, or nil if a match could not be found.
    /// - precondition: `range` must be in the bounds of the Data.
    public func range(of dataToFind: Data, options: Data.SearchOptions = [], in range: Range<Index>? = nil) -> Range<Index>? {
        let nsRange : NSRange
        if let r = range {
            _validateRange(r)
            nsRange = NSRange(location: r.lowerBound - startIndex, length: r.upperBound - r.lowerBound)
        } else {
            nsRange = NSRange(location: 0, length: count)
        }
        let result = _backing.withInteriorPointerReference(_sliceRange) {
            $0.range(of: dataToFind, options: options, in: nsRange)
        }
        if result.location == NSNotFound {
            return nil
        }
        return (result.location + startIndex)..<((result.location + startIndex) + result.length)
    }
    
    /// Enumerate the contents of the data.
    ///
    /// In some cases, (for example, a `Data` backed by a `dispatch_data_t`, the bytes may be stored discontiguously. In those cases, this function invokes the closure for each contiguous region of bytes.
    /// - parameter block: The closure to invoke for each region of data. You may stop the enumeration by setting the `stop` parameter to `true`.
    public func enumerateBytes(_ block: (_ buffer: UnsafeBufferPointer<UInt8>, _ byteIndex: Index, _ stop: inout Bool) -> Void) {
        _backing.enumerateBytes(in: _sliceRange, block)
    }

    @inlinable
    internal mutating func _append<SourceType>(_ buffer : UnsafeBufferPointer<SourceType>) {
        if buffer.isEmpty { return }
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        _backing.replaceBytes(in: NSRange(location: _sliceRange.upperBound, length: _backing.length - (_sliceRange.upperBound - _backing._offset)), with: buffer.baseAddress, length: buffer.count * MemoryLayout<SourceType>.stride)
        _sliceRange = _sliceRange.lowerBound..<(_sliceRange.upperBound + buffer.count * MemoryLayout<SourceType>.stride)
    }
    
    public mutating func append(_ bytes: UnsafePointer<UInt8>, count: Int) {
        if count == 0 { return }
        _append(UnsafeBufferPointer(start: bytes, count: count))
    }
    
    public mutating func append(_ other: Data) {
        other.enumerateBytes { (buffer, _, _) in
            _append(buffer)
        }
    }
    
    /// Append a buffer of bytes to the data.
    ///
    /// - parameter buffer: The buffer of bytes to append. The size is calculated from `SourceType` and `buffer.count`.
    public mutating func append<SourceType>(_ buffer : UnsafeBufferPointer<SourceType>) {
        _append(buffer)
    }

    public mutating func append(contentsOf bytes: [UInt8]) {
        bytes.withUnsafeBufferPointer { (buffer: UnsafeBufferPointer<UInt8>) -> Void in
            _append(buffer)
        }
    }

    @inlinable
    public mutating func append<S : Sequence>(contentsOf newElements: S) where S.Iterator.Element == Iterator.Element {
        let underestimatedCount = Swift.max(newElements.underestimatedCount, 1)
        _withStackOrHeapBuffer(underestimatedCount) { (buffer) in
            let capacity = buffer.pointee.capacity
            let base = buffer.pointee.memory.bindMemory(to: UInt8.self, capacity: capacity)
            var (iter, endIndex) = newElements._copyContents(initializing: UnsafeMutableBufferPointer(start: base, count: capacity))
            _append(UnsafeBufferPointer(start: base, count: endIndex))
            while var element = iter.next() {
                append(&element, count: 1)
            }
        }
    }
    
    // MARK: -
    
    /// Set a region of the data to `0`.
    ///
    /// If `range` exceeds the bounds of the data, then the data is resized to fit.
    /// - parameter range: The range in the data to set to `0`.
    public mutating func resetBytes(in range: Range<Index>) {
        // it is worth noting that the range here may be out of bounds of the Data itself (which triggers a growth)
        precondition(range.lowerBound >= 0, "Ranges must not be negative bounds")
        precondition(range.upperBound >= 0, "Ranges must not be negative bounds")
        let range = NSRange(location: range.lowerBound, length: range.upperBound - range.lowerBound)
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        _backing.resetBytes(in: range)
        if _sliceRange.upperBound < range.upperBound {
            _sliceRange = _sliceRange.lowerBound..<range.upperBound
        }
    }
    
    /// Replace a region of bytes in the data with new data.
    ///
    /// This will resize the data if required, to fit the entire contents of `data`.
    ///
    /// - precondition: The bounds of `subrange` must be valid indices of the collection.
    /// - parameter subrange: The range in the data to replace. If `subrange.lowerBound == data.count && subrange.count == 0` then this operation is an append.
    /// - parameter data: The replacement data.
    public mutating func replaceSubrange(_ subrange: Range<Index>, with data: Data) {
        let cnt = data.count
        data.withUnsafeBytes {
            replaceSubrange(subrange, with: $0, count: cnt)
        }
    }
    
    /// Replace a region of bytes in the data with new bytes from a buffer.
    ///
    /// This will resize the data if required, to fit the entire contents of `buffer`.
    ///
    /// - precondition: The bounds of `subrange` must be valid indices of the collection.
    /// - parameter subrange: The range in the data to replace.
    /// - parameter buffer: The replacement bytes.
    public mutating func replaceSubrange<SourceType>(_ subrange: Range<Index>, with buffer: UnsafeBufferPointer<SourceType>) {
        guard !buffer.isEmpty  else { return }
        replaceSubrange(subrange, with: buffer.baseAddress!, count: buffer.count * MemoryLayout<SourceType>.stride)
    }
    
    /// Replace a region of bytes in the data with new bytes from a collection.
    ///
    /// This will resize the data if required, to fit the entire contents of `newElements`.
    ///
    /// - precondition: The bounds of `subrange` must be valid indices of the collection.
    /// - parameter subrange: The range in the data to replace.
    /// - parameter newElements: The replacement bytes.
    public mutating func replaceSubrange<ByteCollection : Collection>(_ subrange: Range<Index>, with newElements: ByteCollection) where ByteCollection.Iterator.Element == Data.Iterator.Element {
        _validateRange(subrange)
        let totalCount: Int = numericCast(newElements.count)
        _withStackOrHeapBuffer(totalCount) { conditionalBuffer in
            let buffer = UnsafeMutableBufferPointer(start: conditionalBuffer.pointee.memory.assumingMemoryBound(to: UInt8.self), count: totalCount)
            var (iterator, index) = newElements._copyContents(initializing: buffer)
            while let byte = iterator.next() {
                buffer[index] = byte
                index = buffer.index(after: index)
            }
            replaceSubrange(subrange, with: conditionalBuffer.pointee.memory, count: totalCount)
        }
    }
    
    public mutating func replaceSubrange(_ subrange: Range<Index>, with bytes: UnsafeRawPointer, count cnt: Int) {
        _validateRange(subrange)
        let nsRange = NSRange(location: subrange.lowerBound, length: subrange.upperBound - subrange.lowerBound)
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        let upper = _sliceRange.upperBound
        _backing.replaceBytes(in: nsRange, with: bytes, length: cnt)
        let resultingUpper = upper - nsRange.length + cnt
        _sliceRange = _sliceRange.lowerBound..<resultingUpper
    }
    
    /// Return a new copy of the data in a specified range.
    ///
    /// - parameter range: The range to copy.
    public func subdata(in range: Range<Index>) -> Data {
        _validateRange(range)
        if isEmpty {
            return Data()
        }
        return _backing.subdata(in: range)
    }
    
    // MARK: -
    //
    
    /// Returns a Base-64 encoded string.
    ///
    /// - parameter options: The options to use for the encoding. Default value is `[]`.
    /// - returns: The Base-64 encoded string.
    public func base64EncodedString(options: Data.Base64EncodingOptions = []) -> String {
        return _backing.withInteriorPointerReference(_sliceRange) {
            return $0.base64EncodedString(options: options)
        }
    }
    
    /// Returns a Base-64 encoded `Data`.
    ///
    /// - parameter options: The options to use for the encoding. Default value is `[]`.
    /// - returns: The Base-64 encoded data.
    public func base64EncodedData(options: Data.Base64EncodingOptions = []) -> Data {
        return _backing.withInteriorPointerReference(_sliceRange) {
            return $0.base64EncodedData(options: options)
        }
    }
    
    // MARK: -
    //
    
    /// The hash value for the data.
    public var hashValue: Int { // FIXME(hashValue): Remove
        var hashValue = 0
        let hashRange: Range<Int> = _sliceRange.lowerBound..<Swift.min(_sliceRange.lowerBound + 80, _sliceRange.upperBound)
        _withStackOrHeapBuffer(hashRange.count + 1) { buffer in
            if !hashRange.isEmpty {
                _backing.withUnsafeBytes(in: hashRange) {
                    memcpy(buffer.pointee.memory, $0.baseAddress!, hashRange.count)
                }
            }
            hashValue = Int(bitPattern: CFHashBytes(buffer.pointee.memory.assumingMemoryBound(to: UInt8.self), hashRange.count))
        }
        return hashValue
    }

    public func hash(into hasher: inout Hasher) {
        _backing.withUnsafeBytes(in: _sliceRange) { buffer in
            hasher.combine(bytes: buffer)
        }
    }

    public func advanced(by amount: Int) -> Data {
        _validateIndex(startIndex + amount)
        let length = count - amount
        precondition(length > 0)
        return withUnsafeBytes { (ptr: UnsafePointer<UInt8>) -> Data in
            return Data(bytes: ptr.advanced(by: amount), count: length)
        }
    }
    
    // MARK: -
    
    // MARK: -
    // MARK: Index and Subscript
    
    /// Sets or returns the byte at the specified index.
    public subscript(index: Index) -> UInt8 {
        get {
            _validateIndex(index)
            return _backing.get(index)
        }
        set {
            _validateIndex(index)
            if !isKnownUniquelyReferenced(&_backing) {
                _backing = _backing.mutableCopy(_sliceRange)
            }
            _backing.set(index, to: newValue)
        }
    }
    
    public subscript(bounds: Range<Index>) -> Data {
        get {
            _validateRange(bounds)
            return Data(backing: _backing, range: bounds)
        }
        set {
            replaceSubrange(bounds, with: newValue)
        }
    }
    
    public subscript<R: RangeExpression>(_ rangeExpression: R) -> Data
        where R.Bound: FixedWidthInteger {
        get {
            let lower = R.Bound(_sliceRange.lowerBound)
            let upper = R.Bound(_sliceRange.upperBound)
            let range = rangeExpression.relative(to: lower..<upper)
            let start: Int = numericCast(range.lowerBound)
            let end: Int = numericCast(range.upperBound)
            let r: Range<Int> = start..<end
            _validateRange(r)
            return Data(backing: _backing, range: r)
        }
        set {
            let lower = R.Bound(_sliceRange.lowerBound)
            let upper = R.Bound(_sliceRange.upperBound)
            let range = rangeExpression.relative(to: lower..<upper)
            let start: Int = numericCast(range.lowerBound)
            let end: Int = numericCast(range.upperBound)
            let r: Range<Int> = start..<end
            _validateRange(r)
            replaceSubrange(r, with: newValue)
        }
        
    }
    
    /// The start `Index` in the data.
    public var startIndex: Index {
        get {
            return _sliceRange.lowerBound
        }
    }
    
    /// The end `Index` into the data.
    ///
    /// This is the "one-past-the-end" position, and will always be equal to the `count`.
    public var endIndex: Index {
        get {
            return _sliceRange.upperBound
        }
    }
    
    public func index(before i: Index) -> Index {
        return i - 1
    }
    
    public func index(after i: Index) -> Index {
        return i + 1
    }
    
    public var indices: Range<Int> {
        get {
            return startIndex..<endIndex
        }
    }
    
    public func _copyContents(initializing buffer: UnsafeMutableBufferPointer<UInt8>) -> (Iterator, UnsafeMutableBufferPointer<UInt8>.Index) {
        guard !isEmpty else { return (makeIterator(), buffer.startIndex) }
        guard let p = buffer.baseAddress else {
            preconditionFailure("Attempt to copy contents into nil buffer pointer")
        }
        let cnt = count
        precondition(cnt <= buffer.count, "Insufficient space allocated to copy Data contents")
        
        withUnsafeBytes { p.initialize(from: $0, count: cnt) }
        
        return (Iterator(endOf: self), buffer.index(buffer.startIndex, offsetBy: cnt))
    }
    
    /// An iterator over the contents of the data.
    ///
    /// The iterator will increment byte-by-byte.
    public func makeIterator() -> Data.Iterator {
        return Iterator(self)
    }
    
    public struct Iterator : IteratorProtocol {
        // Both _data and _endIdx should be 'let' rather than 'var'.
        // They are 'var' so that the stored properties can be read
        // independently of the other contents of the struct. This prevents
        // an exclusivity violation when reading '_endIdx' and '_data'
        // while simultaneously mutating '_buffer' with the call to
        // withUnsafeMutablePointer(). Once we support accessing struct
        // let properties independently we should make these variables
        // 'let' again.

        private var _data: Data
        private var _buffer: (
        UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8,
        UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8,
        UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8,
        UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
        private var _idx: Data.Index
        private var _endIdx: Data.Index
        
        fileprivate init(_ data: Data) {
            _data = data
            _buffer = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            _idx = data.startIndex
            _endIdx = data.endIndex
        }
        
        fileprivate init(endOf data: Data) {
            self._data = data
            _buffer = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            _idx = data.endIndex
            _endIdx = data.endIndex
        }
        
        public mutating func next() -> UInt8? {
            guard _idx < _endIdx else { return nil }
            defer { _idx += 1 }
            let bufferSize = MemoryLayout.size(ofValue: _buffer)
            return withUnsafeMutablePointer(to: &_buffer) { ptr_ in
                let ptr = UnsafeMutableRawPointer(ptr_).assumingMemoryBound(to: UInt8.self)
                let bufferIdx = (_idx - _data.startIndex) % bufferSize
                if bufferIdx == 0 {
                    // populate the buffer
                    _data.copyBytes(to: ptr, from: _idx..<(_endIdx - _idx > bufferSize ? _idx + bufferSize : _endIdx))
                }
                return ptr[bufferIdx]
            }
        }
    }
    
    // MARK: -
    //
    
    @available(*, unavailable, renamed: "count")
    public var length: Int {
        get { fatalError() }
        set { fatalError() }
    }
    
    @available(*, unavailable, message: "use withUnsafeBytes instead")
    public var bytes: UnsafeRawPointer { fatalError() }
    
    @available(*, unavailable, message: "use withUnsafeMutableBytes instead")
    public var mutableBytes: UnsafeMutableRawPointer { fatalError() }
    
    /// Returns `true` if the two `Data` arguments are equal.
    public static func ==(d1 : Data, d2 : Data) -> Bool {
        let backing1 = d1._backing
        let backing2 = d2._backing
        if backing1 === backing2 {
            if d1._sliceRange == d2._sliceRange {
                return true
            }
        }
        let length1 = d1.count
        if length1 != d2.count {
            return false
        }
        if backing1.bytes == backing2.bytes {
            if d1._sliceRange == d2._sliceRange {
                return true
            }
        }
        if length1 > 0 {
            return d1.withUnsafeBytes { (b1) in
                return d2.withUnsafeBytes { (b2) in
                    return memcmp(b1, b2, length1) == 0
                }
            }
        }
        return true
    }
}


extension Data : CustomStringConvertible, CustomDebugStringConvertible, CustomReflectable {
    /// A human-readable description for the data.
    public var description: String {
        return "\(self.count) bytes"
    }
    
    /// A human-readable debug description for the data.
    public var debugDescription: String {
        return self.description
    }
    
    public var customMirror: Mirror {
        let nBytes = self.count
        var children: [(label: String?, value: Any)] = []
        children.append((label: "count", value: nBytes))
        
        self.withUnsafeBytes { (bytes : UnsafePointer<UInt8>) in
            children.append((label: "pointer", value: bytes))
        }
        
        // Minimal size data is output as an array
        if nBytes < 64 {
            children.append((label: "bytes", value: Array(self[startIndex..<Swift.min(nBytes + startIndex, endIndex)])))
        }
        
        let m = Mirror(self, children:children, displayStyle: Mirror.DisplayStyle.struct)
        return m
    }
}

extension Data {
    @available(*, unavailable, renamed: "copyBytes(to:count:)")
    public func getBytes<UnsafeMutablePointerVoid: _Pointer>(_ buffer: UnsafeMutablePointerVoid, length: Int) { }
    
    @available(*, unavailable, renamed: "copyBytes(to:from:)")
    public func getBytes<UnsafeMutablePointerVoid: _Pointer>(_ buffer: UnsafeMutablePointerVoid, range: NSRange) { }
}

/// Provides bridging functionality for struct Data to class NSData and vice-versa.

extension Data : _ObjectiveCBridgeable {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSData {
        return _backing.bridgedReference(_sliceRange)
    }
    
    public static func _forceBridgeFromObjectiveC(_ input: NSData, result: inout Data?) {
        // We must copy the input because it might be mutable; just like storing a value type in ObjC
        result = Data(referencing: input)
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ input: NSData, result: inout Data?) -> Bool {
        // We must copy the input because it might be mutable; just like storing a value type in ObjC
        result = Data(referencing: input)
        return true
    }

    @_effects(readonly)
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSData?) -> Data {
        guard let src = source else { return Data() }
        return Data(referencing: src)
    }
}

extension NSData : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(Data._unconditionallyBridgeFromObjectiveC(self))
    }
}

extension Data : Codable {
    public init(from decoder: Decoder) throws {
        var container = try decoder.unkeyedContainer()
        
        // It's more efficient to pre-allocate the buffer if we can.
        if let count = container.count {
            self.init(count: count)
            
            // Loop only until count, not while !container.isAtEnd, in case count is underestimated (this is misbehavior) and we haven't allocated enough space.
            // We don't want to write past the end of what we allocated.
            for i in 0 ..< count {
                let byte = try container.decode(UInt8.self)
                self[i] = byte
            }
        } else {
            self.init()
        }
        
        while !container.isAtEnd {
            var byte = try container.decode(UInt8.self)
            self.append(&byte, count: 1)
        }
    }
    
    public func encode(to encoder: Encoder) throws {
        var container = encoder.unkeyedContainer()
        
        // Since enumerateBytes does not rethrow, we need to catch the error, stow it away, and rethrow if we stopped.
        var caughtError: Error? = nil
        self.enumerateBytes { (buffer: UnsafeBufferPointer<UInt8>, byteIndex: Data.Index, stop: inout Bool) in
            do {
                try container.encode(contentsOf: buffer)
            } catch {
                caughtError = error
                stop = true
            }
        }
        
        if let error = caughtError {
            throw error
        }
    }
}
