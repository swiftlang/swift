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

#if os(OSX) || os(iOS)
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

#else

@_exported import Foundation // Clang module

@_silgen_name("__NSDataInvokeDeallocatorVM")
internal func __NSDataInvokeDeallocatorVM(_ mem: UnsafeMutableRawPointer, _ length: Int)

@_silgen_name("__NSDataInvokeDeallocatorUnmap")
internal func __NSDataInvokeDeallocatorUnmap(_ mem: UnsafeMutableRawPointer, _ length: Int)

@_silgen_name("__NSDataInvokeDeallocatorFree")
internal func __NSDataInvokeDeallocatorFree(_ mem: UnsafeMutableRawPointer, _ length: Int)

@_silgen_name("_NSWriteDataToFile_Swift")
internal func _NSWriteDataToFile_Swift(url: NSURL, data: NSData, options: UInt, error: NSErrorPointer) -> Bool

#endif

public final class _DataStorage {
    public enum Backing {
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
    
    public static let maxSize = Int.max >> 1
    public static let vmOpsThreshold = NSPageSize() * 4
    
    public static func allocate(_ size: Int, _ clear: Bool) -> UnsafeMutableRawPointer? {
        if clear {
            return calloc(1, size)
        } else {
            return malloc(size)
        }
    }
    
    
    public static func move(_ dest_: UnsafeMutableRawPointer, _ source_: UnsafeRawPointer?, _ num_: Int) {
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
    
    public static func shouldAllocateCleared(_ size: Int) -> Bool {
        return (size > (128 * 1024))
    }
    
    public var _bytes: UnsafeMutableRawPointer?
    public var _length: Int
    public var _capacity: Int
    public var _needToZero: Bool
    public var _deallocator: ((UnsafeMutableRawPointer, Int) -> Void)?
    public var _backing: Backing = .swift
    
    public var bytes: UnsafeRawPointer? {
        @inline(__always)
        get {
            switch _backing {
            case .swift:
                return UnsafeRawPointer(_bytes)
            case .immutable:
                return UnsafeRawPointer(_bytes)
            case .mutable:
                return UnsafeRawPointer(_bytes)
            case .customReference(let d):
                return d.bytes
            case .customMutableReference(let d):
                return d.bytes
            }
        }
    }
    
    public var mutableBytes: UnsafeMutableRawPointer? {
        @inline(__always)
        get {
            switch _backing {
            case .swift:
                return _bytes
            case .immutable(let d):
                let data = d.mutableCopy() as! NSMutableData
                data.length = length
                _backing = .mutable(data)
                _bytes = data.mutableBytes
                return data.mutableBytes
            case .mutable:
                return _bytes
            case .customReference(let d):
                let data = d.mutableCopy() as! NSMutableData
                data.length = length
                _backing = .customMutableReference(data)
                _bytes = data.mutableBytes
                return data.mutableBytes
            case .customMutableReference(let d):
                return d.mutableBytes
            }
        }
    }
    
    public var length: Int {
        @inline(__always)
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
            }
        }
        @inline(__always)
        set {
            setLength(newValue)
        }
    }
    
    
    public func _freeBytes() {
        if let bytes = _bytes {
            if let dealloc = _deallocator {
                dealloc(bytes, length)
            } else {
                free(bytes)
            }
        }
    }
    
    public func enumerateBytes(_ block: (_ buffer: UnsafeBufferPointer<UInt8>, _ byteIndex: Data.Index, _ stop: inout Bool) -> Void) {
        var stop: Bool = false
        switch _backing {
        case .swift:
            block(UnsafeBufferPointer<UInt8>(start: _bytes?.assumingMemoryBound(to: UInt8.self), count: _length), 0, &stop)
        case .immutable:
            block(UnsafeBufferPointer<UInt8>(start: _bytes?.assumingMemoryBound(to: UInt8.self), count: _length), 0, &stop)
        case .mutable:
            block(UnsafeBufferPointer<UInt8>(start: _bytes?.assumingMemoryBound(to: UInt8.self), count: _length), 0, &stop)
        case .customReference(let d):
            d.enumerateBytes { (ptr, range, stop) in
                var stopv = false
                let bytePtr = ptr.bindMemory(to: UInt8.self, capacity: range.length)
                block(UnsafeBufferPointer(start: bytePtr, count: range.length), range.length, &stopv)
                if stopv {
                    stop.pointee = true
                }
            }
        case .customMutableReference(let d):
            d.enumerateBytes { (ptr, range, stop) in
                var stopv = false
                let bytePtr = ptr.bindMemory(to: UInt8.self, capacity: range.length)
                block(UnsafeBufferPointer(start: bytePtr, count: range.length), range.length, &stopv)
                if stopv {
                    stop.pointee = true
                }
            }
        }
    }
    
    @inline(never)
    public func _grow(_ newLength: Int, _ clear: Bool) {
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
                if newBytes != nil {
                    _DataStorage.move(newBytes!, _bytes!, origLength)
                    _freeBytes()
                }
            }
            /* Where calloc/memmove/free fails, realloc might succeed */
            if newBytes == nil {
                allocateCleared = false
                if _deallocator != nil {
                    newBytes = _DataStorage.allocate(newCapacity, true)
                    if newBytes != nil {
                        _DataStorage.move(newBytes!, _bytes!, origLength)
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
                    if newBytes != nil {
                        _DataStorage.move(newBytes!, _bytes!, origLength)
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
    
    @inline(__always)
    public func setLength(_ length: Int) {
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
        }
    }
    
    @inline(__always)
    public func append(_ bytes: UnsafeRawPointer, length: Int) {
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
            _backing = .customReference(data)
        case .customMutableReference(let d):
            d.append(bytes, length: length)
        }
        
    }
    
    // fast-path for appending directly from another data storage
    @inline(__always)
    public func append(_ otherData: _DataStorage) {
        let otherLength = otherData.length
        if otherLength == 0 { return }
        if let bytes = otherData.bytes {
            append(bytes, length: otherLength)
        }
    }
    
    @inline(__always)
    public func append(_ otherData: Data) {
        otherData.enumerateBytes { (buffer: UnsafeBufferPointer<UInt8>, _, _) in
            append(buffer.baseAddress!, length: buffer.count)
        }
    }
    
    @inline(__always)
    public func increaseLength(by extraLength: Int) {
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
        }
        
    }
    
    @inline(__always)
    public func set(_ index: Int, to value: UInt8) {
        switch _backing {
        case .swift:
            fallthrough
        case .mutable:
            _bytes!.advanced(by: index).assumingMemoryBound(to: UInt8.self).pointee = value
        default:
            var theByte = value
            let range = NSRange(location: index, length: 1)
            replaceBytes(in: range, with: &theByte, length: 1)
        }
        
    }
    
    @inline(__always)
    public func replaceBytes(in range: NSRange, with bytes: UnsafeRawPointer?) {
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
            _DataStorage.move(_bytes!.advanced(by: range.location), bytes!, range.length)
        case .immutable(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.replaceBytes(in: range, withBytes: bytes!)
            _backing = .mutable(data)
            _length = data.length
            _bytes = data.mutableBytes
        case .mutable(let d):
            d.replaceBytes(in: range, withBytes: bytes!)
            _length = d.length
            _bytes = d.mutableBytes
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.replaceBytes(in: range, withBytes: bytes!)
            _backing = .customMutableReference(data)
        case .customMutableReference(let d):
            d.replaceBytes(in: range, withBytes: bytes!)
        }
    }
    
    @inline(__always)
    public func replaceBytes(in range: NSRange, with replacementBytes: UnsafeRawPointer?, length replacementLength: Int) {
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
                if replacementBytes != nil {
                    memmove(mutableBytes! + start, replacementBytes!, replacementLength)
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
            _length = replacementLength
            _bytes = data.mutableBytes
        case .mutable(let d):
            d.replaceBytes(in: range, withBytes: replacementBytes, length: replacementLength)
            _backing = .mutable(d)
            _length = replacementLength
            _bytes = d.mutableBytes
        case .customReference(let d):
            let data = d.mutableCopy() as! NSMutableData
            data.replaceBytes(in: range, withBytes: replacementBytes, length: replacementLength)
            _backing = .customMutableReference(data)
        case .customMutableReference(let d):
            d.replaceBytes(in: range, withBytes: replacementBytes, length: replacementLength)
        }
    }
    
    @inline(__always)
    public func resetBytes(in range: NSRange) {
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
        }
        
    }
    
    
    public convenience init() {
        self.init(capacity: 0)
    }
    
    public init(length: Int) {
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
        setLength(length)
    }
    
    
    public init(capacity capacity_: Int) {
        var capacity = capacity_
        precondition(capacity < _DataStorage.maxSize)
        if _DataStorage.vmOpsThreshold <= capacity {
            capacity = NSRoundUpToMultipleOfPageSize(capacity)
        }
        _length = 0
        _bytes = _DataStorage.allocate(capacity, false)!
        _capacity = capacity
        _needToZero = true
    }
    
    public init(bytes: UnsafeRawPointer?, length: Int) {
        precondition(length < _DataStorage.maxSize)
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
    
    public init(bytes: UnsafeMutableRawPointer?, length: Int, copy: Bool, deallocator: ((UnsafeMutableRawPointer, Int) -> Void)?) {
        precondition(length < _DataStorage.maxSize)
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
    
    public init(immutableReference: NSData) {
        _bytes = UnsafeMutableRawPointer(mutating: immutableReference.bytes)
        _capacity = 0
        _needToZero = false
        _length = immutableReference.length
        _backing = .immutable(immutableReference)
    }
    
    public init(mutableReference: NSMutableData) {
        _bytes = mutableReference.mutableBytes
        _capacity = 0
        _needToZero = false
        _length = mutableReference.length
        _backing = .mutable(mutableReference)
    }
    
    public init(customReference: NSData) {
        _bytes = nil
        _capacity = 0
        _needToZero = false
        _length = 0
        _backing = .customReference(customReference)
    }
    
    public init(customMutableReference: NSMutableData) {
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
    
    @inline(__always)
    public func mutableCopy(_ range: Range<Int>) -> _DataStorage {
        switch _backing {
        case .swift:
            return _DataStorage(bytes: _bytes?.advanced(by: range.lowerBound), length: range.count, copy: true, deallocator: nil)
        case .immutable(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return _DataStorage(mutableReference: d.mutableCopy() as! NSMutableData)
            } else {
                return _DataStorage(mutableReference: d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC().mutableCopy() as! NSMutableData)
            }
        case .mutable(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return _DataStorage(mutableReference: d.mutableCopy() as! NSMutableData)
            } else {
                return _DataStorage(mutableReference: d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC().mutableCopy() as! NSMutableData)
            }
        case .customReference(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return _DataStorage(mutableReference: d.mutableCopy() as! NSMutableData)
            } else {
                return _DataStorage(mutableReference: d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC().mutableCopy() as! NSMutableData)
            }
        case .customMutableReference(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return _DataStorage(mutableReference: d.mutableCopy() as! NSMutableData)
            } else {
                return _DataStorage(mutableReference: d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC().mutableCopy() as! NSMutableData)
            }
        }
    }
    
    public func withInteriorPointerReference<T>(_ range: Range<Int>, _ work: (NSData) throws -> T) rethrows -> T {
        switch _backing {
        case .swift:
            let d = _bytes == nil ? NSData() : NSData(bytesNoCopy: _bytes!.advanced(by: range.lowerBound), length: range.count, freeWhenDone: false)
            return try work(d)
        case .immutable(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return try work(d)
            } else {
                return try work(d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC())
            }
        case .mutable(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return try work(d)
            } else {
                return try work(d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC())
            }
        case .customReference(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return try work(d)
            } else {
                return try work(d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC())
            }
        case .customMutableReference(let d):
            if range.lowerBound == 0 && range.upperBound == _length {
                return try work(d)
            } else {
                return try work(d.subdata(with: NSRange(location: range.lowerBound, length: range.count))._bridgeToObjectiveC())
            }
        }
    }
    
    public func bridgedReference() -> NSData {
        switch _backing {
        case .swift:
            return _NSSwiftData(backing: self)
        case .immutable(let d):
            return d
        case .mutable(let d):
            return d
        case .customReference(let d):
            return d
        case .customMutableReference(let d):
            // Because this is returning an object that may be mutated in the future it needs to create a copy to prevent
            // any further mutations out from under the receiver
            return d.copy() as! NSData
        }
    }
    
    public var hashValue: Int {
        switch _backing {
        case .customReference(let d):
            return d.hash
        case .customMutableReference(let d):
            return d.hash
        default:
            let len = _length
            return Int(bitPattern: CFHashBytes(_bytes?.assumingMemoryBound(to: UInt8.self), Swift.min(len, 80)))
        }
    }
    
    public func subdata(in range: Range<Data.Index>) -> Data {
        switch _backing {
        case .customReference(let d):
            return d.subdata(with: NSRange(location: range.lowerBound, length: range.count))
        case .customMutableReference(let d):
            return d.subdata(with: NSRange(location: range.lowerBound, length: range.count))
        default:
            return Data(bytes: _bytes!.advanced(by: range.lowerBound), count: range.count)
        }
    }
}

internal class _NSSwiftData : NSData {
    var _backing: _DataStorage!
    
    convenience init(backing: _DataStorage) {
        self.init()
        _backing = backing
    }
    
    override var length: Int {
        return _backing.length
    }
    
    override var bytes: UnsafeRawPointer {
        // NSData's byte pointer methods are not annotated for nullability correctly
        // (but assume non-null by the wrapping macro guards). This placeholder value
        // is to work-around this bug. Any indirection to the underlying bytes of an NSData
        // with a length of zero would have been a programmer error anyhow so the actual
        // return value here is not needed to be an allocated value. This is specifically
        // needed to live like this to be source compatible with Swift3. Beyond that point
        // this API may be subject to correction.
        return _backing.bytes ?? UnsafeRawPointer(bitPattern: 0xBAD0)!
    }
    
    override func copy(with zone: NSZone? = nil) -> Any {
        return NSData(bytes: _backing.bytes, length: _backing.length)
    }

#if !DEPLOYMENT_RUNTIME_SWIFT
    @objc
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
    public typealias Indices = CountableRange<Int>
    
    @_versioned internal var _backing : _DataStorage
    @_versioned internal var _sliceRange: Range<Index>
    
    
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
                return { __NSDataInvokeDeallocatorVM($0, $1) }
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
    
    /// Initialize a `Data` with the contents of an Array.
    ///
    /// - parameter bytes: An array of bytes to copy.
    public init(bytes: Array<UInt8>) {
        let count = bytes.count
        _backing = bytes.withUnsafeBufferPointer {
            return _DataStorage(bytes: $0.baseAddress, length: count)
        }
        _sliceRange = 0..<count
    }
    
    /// Initialize a `Data` with the contents of an Array.
    ///
    /// - parameter bytes: An array of bytes to copy.
    public init(bytes: ArraySlice<UInt8>) {
        let count = bytes.count
        _backing = bytes.withUnsafeBufferPointer {
            return _DataStorage(bytes: $0.baseAddress, length: count)
        }
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
        _backing = _DataStorage(bytes: bytes, length: count, copy: false, deallocator: whichDeallocator)
        _sliceRange = 0..<count
    }
    
    /// Initialize a `Data` with the contents of a `URL`.
    ///
    /// - parameter url: The `URL` to read.
    /// - parameter options: Options for the read operation. Default value is `[]`.
    /// - throws: An error in the Cocoa domain, if `url` cannot be read.
    public init(contentsOf url: URL, options: Data.ReadingOptions = []) throws {
        let d = try NSData(contentsOf: url, options: ReadingOptions(rawValue: options.rawValue))
        _backing = _DataStorage(immutableReference: d)
        _sliceRange = 0..<d.length
    }
    
    /// Initialize a `Data` from a Base-64 encoded String using the given options.
    ///
    /// Returns nil when the input is not recognized as valid Base-64.
    /// - parameter base64String: The string to parse.
    /// - parameter options: Encoding options. Default value is `[]`.
    public init?(base64Encoded base64String: String, options: Data.Base64DecodingOptions = []) {
        if let d = NSData(base64Encoded: base64String, options: Base64DecodingOptions(rawValue: options.rawValue)) {
            _backing = _DataStorage(immutableReference: d)
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
    public init?(base64Encoded base64Data: Data, options: Data.Base64DecodingOptions = []) {
        if let d = NSData(base64Encoded: base64Data, options: Base64DecodingOptions(rawValue: options.rawValue)) {
            _backing = _DataStorage(immutableReference: d)
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
    public init(referencing reference: NSData) {
#if DEPLOYMENT_RUNTIME_SWIFT
        let providesConcreteBacking = reference._providesConcreteBacking()
#else
        let providesConcreteBacking = (reference as AnyObject)._providesConcreteBacking?() ?? false
#endif
        if providesConcreteBacking {
            _backing = _DataStorage(immutableReference: reference.copy() as! NSData)
            _sliceRange = 0..<reference.length
        } else {
            _backing = _DataStorage(customReference: reference.copy() as! NSData)
            _sliceRange = 0..<reference.length
        }

    }
    
    @_versioned
    internal init(backing: _DataStorage, range: Range<Index>) {
        _backing = backing
        _sliceRange = range
    }
    
    // -----------------------------------
    // MARK: - Properties and Functions
    
    /// The number of bytes in the data.
    
    public var count: Int {
        @inline(__always)
        get {
            return _sliceRange.count
        }
        @inline(__always)
        set {
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
    @inline(__always)
    public func withUnsafeBytes<ResultType, ContentType>(_ body: (UnsafePointer<ContentType>) throws -> ResultType) rethrows -> ResultType {
        let bytes =  _backing.bytes ?? UnsafeRawPointer(bitPattern: 0xBAD0)!
        let contentPtr = bytes.bindMemory(to: ContentType.self, capacity: count / MemoryLayout<ContentType>.stride)
        return try body(contentPtr)
    }
    
    
    /// Mutate the bytes in the data.
    ///
    /// This function assumes that you are mutating the contents.
    /// - warning: The byte pointer argument should not be stored and used outside of the lifetime of the call to the closure.
    @inline(__always)
    public mutating func withUnsafeMutableBytes<ResultType, ContentType>(_ body: (UnsafeMutablePointer<ContentType>) throws -> ResultType) rethrows -> ResultType {
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        let mutableBytes = _backing.mutableBytes ?? UnsafeMutableRawPointer(bitPattern: 0xBAD0)!
        let contentPtr = mutableBytes.bindMemory(to: ContentType.self, capacity: count / MemoryLayout<ContentType>.stride)
        return try body(UnsafeMutablePointer(contentPtr))
    }
    
    // MARK: -
    // MARK: Copy Bytes
    
    /// Copy the contents of the data to a pointer.
    ///
    /// - parameter pointer: A pointer to the buffer you wish to copy the bytes into.
    /// - parameter count: The number of bytes to copy.
    /// - warning: This method does not verify that the contents at pointer have enough space to hold `count` bytes.
    @inline(__always)
    public func copyBytes(to pointer: UnsafeMutablePointer<UInt8>, count: Int) {
        if count == 0 { return }
        memcpy(UnsafeMutableRawPointer(pointer), _backing.bytes!.advanced(by: _sliceRange.lowerBound), count)
    }
    
    @inline(__always)
    private func _copyBytesHelper(to pointer: UnsafeMutableRawPointer, from range: NSRange) {
        if range.length == 0 { return }
        memcpy(UnsafeMutableRawPointer(pointer), _backing.bytes!.advanced(by: range.location + _sliceRange.lowerBound), range.length)
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
            precondition(r.lowerBound >= 0)
            precondition(r.lowerBound < cnt, "The range is outside the bounds of the data")
            
            precondition(r.upperBound >= 0)
            precondition(r.upperBound <= cnt, "The range is outside the bounds of the data")
            
            copyRange = r.lowerBound..<(r.lowerBound + Swift.min(buffer.count * MemoryLayout<DestinationType>.stride, r.count))
        } else {
            copyRange = 0..<Swift.min(buffer.count * MemoryLayout<DestinationType>.stride, cnt)
        }
        
        guard !copyRange.isEmpty else { return 0 }
        
        let nsRange = NSMakeRange(copyRange.lowerBound, copyRange.upperBound - copyRange.lowerBound)
        _copyBytesHelper(to: buffer.baseAddress!, from: nsRange)
        return copyRange.count
    }
    
    // MARK: -
#if !DEPLOYMENT_RUNTIME_SWIFT
    @inline(__always)
    private func _shouldUseNonAtomicWriteReimplementation(options: Data.WritingOptions = []) -> Bool {
        
        // Avoid a crash that happens on OS X 10.11.x and iOS 9.x or before when writing a bridged Data non-atomically with Foundation's standard write() implementation.
        if !options.contains(.atomic) {
            #if os(OSX)
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
                var error : NSError?
                if !_NSWriteDataToFile_Swift(url: url._bridgeToObjectiveC(), data: $0, options: options.rawValue, error: &error) {
                    throw error!
                }
            } else {
                try $0.write(to: url, options: WritingOptions(rawValue: options.rawValue))
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
            nsRange = NSMakeRange(r.lowerBound, r.upperBound - r.lowerBound)
        } else {
            nsRange = NSMakeRange(0, _backing.length)
        }
        let result = _backing.withInteriorPointerReference(_sliceRange) {
            $0.range(of: dataToFind, options: options, in: nsRange)
        }
        if result.location == NSNotFound {
            return nil
        }
        return result.location..<(result.location + result.length)
    }
    
    /// Enumerate the contents of the data.
    ///
    /// In some cases, (for example, a `Data` backed by a `dispatch_data_t`, the bytes may be stored discontiguously. In those cases, this function invokes the closure for each contiguous region of bytes.
    /// - parameter block: The closure to invoke for each region of data. You may stop the enumeration by setting the `stop` parameter to `true`.
    public func enumerateBytes(_ block: (_ buffer: UnsafeBufferPointer<UInt8>, _ byteIndex: Index, _ stop: inout Bool) -> Void) {
        _backing.enumerateBytes(block)
    }
    
    @inline(__always)
    public mutating func append(_ bytes: UnsafePointer<UInt8>, count: Int) {
        if count == 0 { return }
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        _backing.append(bytes, length: count)
        _sliceRange = _sliceRange.lowerBound..<(_sliceRange.upperBound + count)
    }
    
    @inline(__always)
    public mutating func append(_ other: Data) {
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        _backing.append(other._backing)
        _sliceRange = _sliceRange.lowerBound..<(_sliceRange.upperBound + other.count)
    }
    
    /// Append a buffer of bytes to the data.
    ///
    /// - parameter buffer: The buffer of bytes to append. The size is calculated from `SourceType` and `buffer.count`.
    @inline(__always)
    public mutating func append<SourceType>(_ buffer : UnsafeBufferPointer<SourceType>) {
        if buffer.count == 0 { return }
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        _backing.append(buffer.baseAddress!, length: buffer.count * MemoryLayout<SourceType>.stride)
        _sliceRange = _sliceRange.lowerBound..<(_sliceRange.upperBound + buffer.count * MemoryLayout<SourceType>.stride)
    }
    
    @inline(__always)
    public mutating func append(_ other: MutableRangeReplaceableRandomAccessSlice<Data>) {
        let count = other.count
        if count == 0 { return }
        other.base.withUnsafeBytes { (bytes: UnsafePointer<UInt8>) -> Void in
            append(bytes, count: count)
        }
    }
    
    @inline(__always)
    public mutating func append<S : Sequence>(contentsOf newElements: S) where S.Iterator.Element == Iterator.Element {
        let estimatedCount = newElements.underestimatedCount
        var idx = count
        count += estimatedCount
        for byte in newElements {
            let newIndex = idx + 1
            if newIndex > count {
                count = newIndex
            }
            self[idx] = byte
            idx = newIndex
        }
    }
    
    @inline(__always)
    public mutating func append(contentsOf bytes: [UInt8]) {
        bytes.withUnsafeBufferPointer { (buffer: UnsafeBufferPointer<UInt8>) -> Void in
            append(buffer)
        }
    }
    
    // MARK: -
    
    /// Set a region of the data to `0`.
    ///
    /// If `range` exceeds the bounds of the data, then the data is resized to fit.
    /// - parameter range: The range in the data to set to `0`.
    @inline(__always)
    public mutating func resetBytes(in range: Range<Index>) {
        let range = NSMakeRange(range.lowerBound, range.upperBound - range.lowerBound)
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        _backing.resetBytes(in: range)
        if _sliceRange.count < range.location + range.length {
            let newLength = range.location + range.length
            _sliceRange = _sliceRange.lowerBound..<(_sliceRange.lowerBound + newLength)
        }
    }
    
    /// Replace a region of bytes in the data with new data.
    ///
    /// This will resize the data if required, to fit the entire contents of `data`.
    ///
    /// - precondition: The bounds of `subrange` must be valid indices of the collection.
    /// - parameter subrange: The range in the data to replace. If `subrange.lowerBound == data.count && subrange.count == 0` then this operation is an append.
    /// - parameter data: The replacement data.
    @inline(__always)
    public mutating func replaceSubrange(_ subrange: Range<Index>, with data: Data) {
        let nsRange = NSMakeRange(subrange.lowerBound, subrange.upperBound - subrange.lowerBound)
        let cnt = data.count
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        data.withUnsafeBytes { (bytes: UnsafePointer<UInt8>) -> Void in
            let currentLength = _backing.length
            _backing.replaceBytes(in: nsRange, with: bytes, length: cnt)
            let resultingLength = currentLength - nsRange.length + cnt
            _sliceRange = _sliceRange.lowerBound..<(_sliceRange.lowerBound + resultingLength)
        }
    }
    
    /// Replace a region of bytes in the data with new bytes from a buffer.
    ///
    /// This will resize the data if required, to fit the entire contents of `buffer`.
    ///
    /// - precondition: The bounds of `subrange` must be valid indices of the collection.
    /// - parameter subrange: The range in the data to replace.
    /// - parameter buffer: The replacement bytes.
    @inline(__always)
    public mutating func replaceSubrange<SourceType>(_ subrange: Range<Index>, with buffer: UnsafeBufferPointer<SourceType>) {
        let nsRange = NSMakeRange(subrange.lowerBound, subrange.upperBound - subrange.lowerBound)
        let bufferCount = buffer.count * MemoryLayout<SourceType>.stride
        
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        let currentLength = _backing.length
        _backing.replaceBytes(in: nsRange, with: buffer.baseAddress, length: bufferCount)
        let resultingLength = currentLength - nsRange.length + bufferCount
        _sliceRange = _sliceRange.lowerBound..<(_sliceRange.lowerBound + resultingLength)
    }
    
    /// Replace a region of bytes in the data with new bytes from a collection.
    ///
    /// This will resize the data if required, to fit the entire contents of `newElements`.
    ///
    /// - precondition: The bounds of `subrange` must be valid indices of the collection.
    /// - parameter subrange: The range in the data to replace.
    /// - parameter newElements: The replacement bytes.
    @inline(__always)
    public mutating func replaceSubrange<ByteCollection : Collection>(_ subrange: Range<Index>, with newElements: ByteCollection)
        where ByteCollection.Iterator.Element == Data.Iterator.Element {
            
            // Calculate this once, it may not be O(1)
            let replacementCount: Int = numericCast(newElements.count)
            let currentCount = self.count
            let subrangeCount = subrange.count
            
            if currentCount < subrange.lowerBound + subrangeCount {
                if subrangeCount == 0 {
                    preconditionFailure("location \(subrange.lowerBound) exceeds data count \(currentCount)")
                } else {
                    preconditionFailure("range \(subrange) exceeds data count \(currentCount)")
                }
            }
            
            let resultCount = currentCount - subrangeCount + replacementCount
            if resultCount != currentCount {
                // This may realloc.
                // In the future, if we keep the malloced pointer and count inside this struct/ref instead of deferring to NSData, we may be able to do this more efficiently.
                self.count = resultCount
            }

            let shift = resultCount - currentCount
            let start = subrange.lowerBound
            
            self.withUnsafeMutableBytes { (bytes : UnsafeMutablePointer<UInt8>) -> Void in
                if shift != 0 {
                    let destination = bytes + start + replacementCount
                    let source = bytes + start + subrangeCount
                    memmove(destination, source, currentCount - start - subrangeCount)
                }
                
                if replacementCount != 0 {
                    let buf = UnsafeMutableBufferPointer(start: bytes + start, count: replacementCount)
                    var (it,idx) = newElements._copyContents(initializing: buf)
                    precondition(it.next() == nil && idx == buf.endIndex, "newElements iterator returned different count to newElements.count")
                }
            }
    }
    
    @inline(__always)
    public mutating func replaceSubrange(_ subrange: Range<Index>, with bytes: UnsafeRawPointer, count cnt: Int) {
        let nsRange = NSMakeRange(subrange.lowerBound, subrange.upperBound - subrange.lowerBound)
        if !isKnownUniquelyReferenced(&_backing) {
            _backing = _backing.mutableCopy(_sliceRange)
        }
        let currentLength = _backing.length
        _backing.replaceBytes(in: nsRange, with: bytes, length: cnt)
        let resultingLength = currentLength - nsRange.length + cnt
        _sliceRange = _sliceRange.lowerBound..<(_sliceRange.lowerBound + resultingLength)
    }

    /// Return a new copy of the data in a specified range.
    ///
    /// - parameter range: The range to copy.
    @inline(__always)
    public func subdata(in range: Range<Index>) -> Data {
        let length = count
        if count == 0 {
            return Data()
        }
        precondition(length >= range.upperBound)
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
    public var hashValue: Int {
        return _backing.hashValue
    }
    
    @inline(__always)
    public func advanced(by amount: Int) -> Data {
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
        @inline(__always)
        get {
            return withUnsafeBytes { (bytes: UnsafePointer<UInt8>) -> UInt8 in
                return bytes.advanced(by: index).pointee
            }
        }
        @inline(__always)
        set {
            if !isKnownUniquelyReferenced(&_backing) {
                _backing = _backing.mutableCopy(_sliceRange)
            }
            _backing.set(index, to: newValue)
        }
    }
    
    public subscript(bounds: Range<Index>) -> Data {
        @inline(__always)
        get {
            return Data(backing: _backing, range: bounds)
        }
        @inline(__always)
        set {
            replaceSubrange(bounds, with: newValue)
        }
    }
    
    
    /// The start `Index` in the data.
    public var startIndex: Index {
        @inline(__always)
        get {
            return _sliceRange.lowerBound
        }
    }
    
    /// The end `Index` into the data.
    ///
    /// This is the "one-past-the-end" position, and will always be equal to the `count`.
    public var endIndex: Index {
        @inline(__always)
        get {
            return _sliceRange.upperBound
        }
    }
    
    @inline(__always)
    public func index(before i: Index) -> Index {
        return i - 1
    }
    
    @inline(__always)
    public func index(after i: Index) -> Index {
        return i + 1
    }
    
    public var indices: CountableRange<Int> {
        @inline(__always)
        get {
            return startIndex..<endIndex
        }
    }
    
    /// An iterator over the contents of the data.
    ///
    /// The iterator will increment byte-by-byte.
    public func makeIterator() -> Data.Iterator {
        return Iterator(_data: self)
    }
    
    public struct Iterator : IteratorProtocol {
        private let _data: Data
        private var _buffer: (
        UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8,
        UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8,
        UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8,
        UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8)
        private var _idx: Data.Index
        private let _endIdx: Data.Index
        
        fileprivate init(_data: Data) {
            self._data = _data
            _buffer = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
            _idx = _data.startIndex
            _endIdx = _data.endIndex
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
            return true
        }
        let length1 = backing1.length
        if length1 != backing2.length {
            return false
        }
        if backing1.bytes == backing2.bytes {
            return true
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
            children.append((label: "bytes", value: Array(self[0..<nBytes])))
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

#if DEPLOYMENT_RUNTIME_SWIFT
internal typealias DataBridgeType = _ObjectTypeBridgeable
#else
internal typealias DataBridgeType = _ObjectiveCBridgeable
#endif

extension Data : DataBridgeType {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSData {
        return _backing.bridgedReference()
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
    
    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSData?) -> Data {
        var result: Data?
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NSData : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(Data._unconditionallyBridgeFromObjectiveC(self))
    }
}
