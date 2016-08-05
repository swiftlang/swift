//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

@_silgen_name("__NSDataInvokeDeallocatorVM")
internal func __NSDataInvokeDeallocatorVM(_ mem: UnsafeMutableRawPointer, _ length: Int) -> Void

@_silgen_name("__NSDataInvokeDeallocatorUnmap")
internal func __NSDataInvokeDeallocatorUnmap(_ mem: UnsafeMutableRawPointer, _ length: Int) -> Void

@_silgen_name("__NSDataInvokeDeallocatorFree")
internal func __NSDataInvokeDeallocatorFree(_ mem: UnsafeMutableRawPointer, _ length: Int) -> Void

@_silgen_name("_NSWriteDataToFile_Swift")
internal func _NSWriteDataToFile_Swift(url: URL, data: NSData, options: UInt, error: NSErrorPointer) -> Bool

internal final class _SwiftNSData : _SwiftNativeNSData, _SwiftNativeFoundationType {
    internal typealias ImmutableType = NSData
    internal typealias MutableType = NSMutableData
    
    var __wrapped : _MutableUnmanagedWrapper<ImmutableType, MutableType>
    
    init(immutableObject: AnyObject) {
        // Take ownership.
        __wrapped = .Immutable(Unmanaged.passRetained(_unsafeReferenceCast(immutableObject, to: ImmutableType.self)))
        
        super.init()
    }
    
    init(mutableObject: AnyObject) {
        // Take ownership.
        __wrapped = .Mutable(Unmanaged.passRetained(_unsafeReferenceCast(mutableObject, to: MutableType.self)))
        super.init()
    }
    
    internal required init(unmanagedImmutableObject: Unmanaged<ImmutableType>) {
        // Take ownership.
        __wrapped = .Immutable(unmanagedImmutableObject)
        
        super.init()
    }
    
    internal required init(unmanagedMutableObject: Unmanaged<MutableType>) {
        // Take ownership.
        __wrapped = .Mutable(unmanagedMutableObject)
        
        super.init()
    }
    
    deinit {
        releaseWrappedObject()
    }
}


/**
 `Data` is a `MutableCollection` of bytes.
 
 This type provides "copy-on-write" behavior, and is also bridged to the Objective-C `NSData` class. You can wrap an instance of a custom subclass of `NSData` in `struct Data` by converting it using `myData as Data`.
 
 `Data` can be initialized with an `UnsafeRawPointer` and count, an array of `UInt8` (the primitive byte type), an `UnsafeBufferPointer`, the contents of a file, or base-64 encoded data or strings. The buffer-oriented functions provide an extra measure of safety by automatically performing the size calculation, as the type is known at compile time.
 */
public struct Data : ReferenceConvertible, Equatable, Hashable, RandomAccessCollection, MutableCollection, RangeReplaceableCollection, _MutablePairBoxing {
    /// The Objective-C bridged type of `Data`.
    public typealias ReferenceType = NSData
    
    public typealias ReadingOptions = NSData.ReadingOptions
    public typealias WritingOptions = NSData.WritingOptions
    public typealias SearchOptions = NSData.SearchOptions
    public typealias Base64EncodingOptions = NSData.Base64EncodingOptions
    public typealias Base64DecodingOptions = NSData.Base64DecodingOptions
    
    public typealias Index = Int
    public typealias Indices = DefaultRandomAccessIndices<Data>
    
    internal var _wrapped : _SwiftNSData
    
    /// A standard or custom deallocator for `Data`.
    ///
    /// When creating a `Data` with the no-copy initializer, you may specify a `Data.Deallocator` to customize the behavior of how the backing store is deallocated.
    public enum Deallocator {
        /// Use a virtual memory deallocator.
        case virtualMemory

        /// Use `munmap`.
        case unmap

        /// Use `free`.
        case free

        /// Do nothing upon deallocation.
        case none

        /// A custom deallocator.
        case custom((UnsafeMutableRawPointer, Int) -> Void)
        
        fileprivate var _deallocator : ((UnsafeMutableRawPointer, Int) -> Void)? {
            switch self {
            case .virtualMemory:
                return { __NSDataInvokeDeallocatorVM($0, $1) }
            case .unmap:
                return { __NSDataInvokeDeallocatorUnmap($0, $1) }
            case .free:
                return { __NSDataInvokeDeallocatorFree($0, $1) }
            case .none:
                return nil
            case .custom(let b):
                return { (ptr, len) in
                    b(ptr, len)
                }
            }
        }
    }
    
    // MARK: -
    // MARK: Init methods
    
    /// Initialize a `Data` with copied memory content.
    ///
    /// - parameter bytes: A pointer to the memory. It will be copied.
    /// - parameter count: The number of bytes to copy.
    public init(bytes: UnsafeRawPointer, count: Int) {
        _wrapped = _SwiftNSData(immutableObject: NSData(bytes: bytes, length: count))
    }

    /// Initialize a `Data` with copied memory content.
    /// 
    /// - parameter buffer: A buffer pointer to copy. The size is calculated from `SourceType` and `buffer.count`.
    public init<SourceType>(buffer: UnsafeBufferPointer<SourceType>) {
        _wrapped = _SwiftNSData(immutableObject: NSData(bytes: buffer.baseAddress, length: MemoryLayout<SourceType>.stride * buffer.count))
    }

    /// Initialize a `Data` with copied memory content.
    ///
    /// - parameter buffer: A buffer pointer to copy. The size is calculated from `SourceType` and `buffer.count`.
    public init<SourceType>(buffer: UnsafeMutableBufferPointer<SourceType>) {
        _wrapped = _SwiftNSData(immutableObject: NSData(bytes: UnsafePointer(buffer.baseAddress), length: MemoryLayout<SourceType>.stride * buffer.count))
    }

    /// Initialize a `Data` with the contents of an Array.
    ///
    /// - parameter bytes: An array of bytes to copy.
    public init(bytes: Array<UInt8>) {
        _wrapped = bytes.withUnsafeBufferPointer {
            return _SwiftNSData(immutableObject: NSData(bytes: $0.baseAddress, length: $0.count))
        }
    }
    
    /// Initialize a `Data` with the contents of an Array.
    ///
    /// - parameter bytes: An array of bytes to copy.
    public init(bytes: ArraySlice<UInt8>) {
        _wrapped = bytes.withUnsafeBufferPointer {
            return _SwiftNSData(immutableObject: NSData(bytes: $0.baseAddress, length: $0.count))
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
        if let d = NSMutableData(capacity: capacity) {
            _wrapped = _SwiftNSData(mutableObject: d)
        } else {
            fatalError("Unable to allocate data of the requested capacity")
        }
    }
    
    /// Initialize a `Data` with the specified count of zeroed bytes.
    ///
    /// - parameter count: The number of bytes the data initially contains.
    public init(count: Int) {
        if let d = NSMutableData(length: count) {
            _wrapped = _SwiftNSData(mutableObject: d)
        } else {
            fatalError("Unable to allocate data of the requested count")
        }
    }


    /// Initialize an empty `Data`.
    public init() {
        _wrapped = _SwiftNSData(immutableObject: NSData(bytes: nil, length: 0))
    }
    
    /// Initialize a `Data` without copying the bytes.
    ///
    /// If the result is mutated and is not a unique reference, then the `Data` will still follow copy-on-write semantics. In this case, the copy will use its own deallocator. Therefore, it is usually best to only use this initializer when you either enforce immutability with `let` or ensure that no other references to the underlying data are formed.
    /// - parameter bytes: A pointer to the bytes.
    /// - parameter count: The size of the bytes.
    /// - parameter deallocator: Specifies the mechanism to free the indicated buffer, or `.none`.
    public init(bytesNoCopy bytes: UnsafeMutableRawPointer, count: Int, deallocator: Deallocator) {
        let whichDeallocator = deallocator._deallocator
        _wrapped = _SwiftNSData(immutableObject: NSData(bytesNoCopy: bytes, length: count, deallocator: whichDeallocator))
    }
   
    /// Initialize a `Data` with the contents of a `URL`.
    ///
    /// - parameter url: The `URL` to read.
    /// - parameter options: Options for the read operation. Default value is `[]`.
    /// - throws: An error in the Cocoa domain, if `url` cannot be read.
    public init(contentsOf url: URL, options: Data.ReadingOptions = []) throws {
        try _wrapped = _SwiftNSData(immutableObject: NSData(contentsOf: url, options: ReadingOptions(rawValue: options.rawValue)))
    }
    
    /// Initialize a `Data` from a Base-64 encoded String using the given options. 
    ///
    /// Returns nil when the input is not recognized as valid Base-64.
    /// - parameter base64String: The string to parse.
    /// - parameter options: Encoding options. Default value is `[]`.
    public init?(base64Encoded base64String: String, options: Data.Base64DecodingOptions = []) {
        if let d = NSData(base64Encoded: base64String, options: Base64DecodingOptions(rawValue: options.rawValue)) {
            _wrapped = _SwiftNSData(immutableObject: d)
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
            _wrapped = _SwiftNSData(immutableObject: d)
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
        _wrapped = _SwiftNSData(immutableObject: reference.copy() as AnyObject)
    }
    
    // -----------------------------------
    // MARK: - Properties and Functions
    
    /// The number of bytes in the data.
    public var count : Int {
        get {
            return _mapUnmanaged { $0.length }
        }
        set {
            _applyUnmanagedMutation { $0.length = newValue }
        }
    }


    private func _getUnsafeBytesPointer() -> UnsafeRawPointer {
        return _mapUnmanaged { return $0.bytes }
    }
    
    /// Access the bytes in the data.
    ///
    /// - warning: The byte pointer argument should not be stored and used outside of the lifetime of the call to the closure.
    public func withUnsafeBytes<ResultType, ContentType>(_ body: (UnsafePointer<ContentType>) throws -> ResultType) rethrows -> ResultType {
        let bytes =  _getUnsafeBytesPointer()
        defer { _fixLifetime(self)}
        let contentPtr = bytes.bindMemory(to: ContentType.self, capacity: count / MemoryLayout<ContentType>.stride)
        return try body(contentPtr)
    }
    
    private mutating func _getUnsafeMutableBytesPointer() -> UnsafeMutableRawPointer {
        return _applyUnmanagedMutation {
            return $0.mutableBytes
        }
    }
    
    /// Mutate the bytes in the data.
    ///
    /// This function assumes that you are mutating the contents.
    /// - warning: The byte pointer argument should not be stored and used outside of the lifetime of the call to the closure.
    public mutating func withUnsafeMutableBytes<ResultType, ContentType>(_ body: (UnsafeMutablePointer<ContentType>) throws -> ResultType) rethrows -> ResultType {
        let mutableBytes = _getUnsafeMutableBytesPointer()
        defer { _fixLifetime(self)}
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
    public func copyBytes(to pointer: UnsafeMutablePointer<UInt8>, count: Int) {
        _mapUnmanaged { $0.getBytes(pointer, length: count) }
    }
    
    private func _copyBytesHelper(to pointer: UnsafeMutableRawPointer, from range: NSRange) {
        _mapUnmanaged { $0.getBytes(pointer, range: range) }
    }
    
    /// Copy a subset of the contents of the data to a pointer.
    ///
    /// - parameter pointer: A pointer to the buffer you wish to copy the bytes into.
    /// - parameter range: The range in the `Data` to copy.
    /// - warning: This method does not verify that the contents at pointer have enough space to hold the required number of bytes.
    public func copyBytes(to pointer: UnsafeMutablePointer<UInt8>, from range: Range<Index>) {
        _copyBytesHelper(to: pointer, from: NSRange(range))
    }
    
    /// Copy the contents of the data into a buffer.
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
    
    private func _shouldUseNonAtomicWriteReimplementation(options: Data.WritingOptions = []) -> Bool {

        // Avoid a crash that happens on OSX 10.11.x and iOS 9.x or before when writing a bridged Data non-atomically with Foundation's standard write() implementation.
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
   
    /// Write the contents of the `Data` to a location.
    ///
    /// - parameter url: The location to write the data into.
    /// - parameter options: Options for writing the data. Default value is `[]`. 
    /// - throws: An error in the Cocoa domain, if there is an error writing to the `URL`.
    public func write(to url: URL, options: Data.WritingOptions = []) throws {
        try _mapUnmanaged {
            if _shouldUseNonAtomicWriteReimplementation(options: options) {
                var error : NSError?
                if !_NSWriteDataToFile_Swift(url: url, data: $0, options: options.rawValue, error: &error) {
                    throw error!
                }
            } else {
                try $0.write(to: url, options: WritingOptions(rawValue: options.rawValue))
            }
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
            nsRange = NSMakeRange(0, _wrapped.length)
        }
        let result = _mapUnmanaged {
            $0.range(of: dataToFind, options: SearchOptions(rawValue: options.rawValue), in: nsRange)
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
        _mapUnmanaged {
            $0.enumerateBytes { (ptr, range, stop) in
                var stopv = false
                let bytePtr = ptr.bindMemory(to: UInt8.self, capacity: range.length)
                block(UnsafeBufferPointer(start: bytePtr, count: range.length), range.length, &stopv)
                if stopv {
                    stop.pointee = true
                }
            }
        }
    }
    
    /// Append bytes to the data.
    ///
    /// - parameter bytes: A pointer to the bytes to copy in to the data.
    /// - parameter count: The number of bytes to copy.
    public mutating func append(_ bytes: UnsafePointer<UInt8>, count: Int) {
        _applyUnmanagedMutation {
            $0.append(bytes, length: count)
        }
    }
    
    /// Append data to the data.
    ///
    /// - parameter data: The data to append to this data.
    public mutating func append(_ other: Data) {
        _applyUnmanagedMutation { $0.append(other) }
    }
    
    /// Append a buffer of bytes to the data.
    ///
    /// - parameter buffer: The buffer of bytes to append. The size is calculated from `SourceType` and `buffer.count`.
    public mutating func append<SourceType>(_ buffer : UnsafeBufferPointer<SourceType>) {
        _applyUnmanagedMutation {
            $0.append(buffer.baseAddress!, length: buffer.count * MemoryLayout<SourceType>.stride)
        }
    }
    
    
    // MARK: -
   
    /// Set a region of the data to `0`.
    ///
    /// If `range` exceeds the bounds of the data, then the data is resized to fit.
    /// - parameter range: The range in the data to set to `0`. 
    public mutating func resetBytes(in range: Range<Index>) {
        let range = NSMakeRange(range.lowerBound, range.upperBound - range.lowerBound)
        _applyUnmanagedMutation {
            $0.resetBytes(in: range)
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
        let nsRange = NSMakeRange(subrange.lowerBound, subrange.upperBound - subrange.lowerBound)
        let cnt = data.count
        let bytes = data._getUnsafeBytesPointer()
        
        _applyUnmanagedMutation {
            $0.replaceBytes(in: nsRange, withBytes: bytes, length: cnt)
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
        let nsRange = NSMakeRange(subrange.lowerBound, subrange.upperBound - subrange.lowerBound)
        let bufferCount = buffer.count * MemoryLayout<SourceType>.stride
        
        _applyUnmanagedMutation {
            $0.replaceBytes(in: nsRange, withBytes: buffer.baseAddress, length: bufferCount)
        }

    }
    
    /// Replace a region of bytes in the data with new bytes from a collection.
    ///
    /// This will resize the data if required, to fit the entire contents of `newElements`.
    ///
    /// - precondition: The bounds of `subrange` must be valid indices of the collection.
    /// - parameter subrange: The range in the data to replace.
    /// - parameter newElements: The replacement bytes.
    public mutating func replaceSubrange<ByteCollection : Collection>(_ subrange: Range<Index>, with newElements: ByteCollection)
      where ByteCollection.Iterator.Element == Data.Iterator.Element {
        
        // Calculate this once, it may not be O(1)
        let replacementCount : Int = numericCast(newElements.count)
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

        self.withUnsafeMutableBytes { (bytes : UnsafeMutablePointer<UInt8>) -> () in
            if shift != 0 {
                let destination = bytes + start + replacementCount
                let source = bytes + start + subrangeCount
                memmove(destination, source, currentCount - start - subrangeCount)
            }
            
            if replacementCount != 0 {
                newElements._copyContents(initializing: bytes + start)
            }
        }
    }

    
    /// Return a new copy of the data in a specified range.
    ///
    /// - parameter range: The range to copy.
    public func subdata(in range: Range<Index>) -> Data {
        let nsRange = NSMakeRange(range.lowerBound, range.upperBound - range.lowerBound)
        return _mapUnmanaged { $0.subdata(with: nsRange) }
    }
    
    // MARK: -
    //
    
    /// Returns a Base-64 encoded string.
    ///
    /// - parameter options: The options to use for the encoding. Default value is `[]`.
    /// - returns: The Base-64 encoded string.
    public func base64EncodedString(options: Data.Base64EncodingOptions = []) -> String {
        return _mapUnmanaged { $0.base64EncodedString(options: options) }
    }
    
    /// Returns a Base-64 encoded `Data`.
    ///
    /// - parameter options: The options to use for the encoding. Default value is `[]`.
    /// - returns: The Base-64 encoded data.
    public func base64EncodedData(options: Data.Base64EncodingOptions = []) -> Data {
        return _mapUnmanaged { $0.base64EncodedData(options: options) }
    }
    
    // MARK: -
    //
    
    /// The hash value for the data.
    public var hashValue: Int {
        return _mapUnmanaged { $0.hashValue }
    }
    
    // MARK: -
    
    // MARK: -
    // MARK: Index and Subscript
   
    /// Sets or returns the byte at the specified index. 
    public subscript(index: Index) -> UInt8 {
        get {
            var result : UInt8 = 0
            copyBytes(to: &result, from: (index ..< index + 1))
            return result
        }
        set {
            let range = NSMakeRange(index, 1)
            var theByte = newValue
            _applyUnmanagedMutation {
                $0.replaceBytes(in: range, withBytes: &theByte, length: 1)
            }
        }
    }
    
    public subscript(bounds: Range<Index>) -> MutableRandomAccessSlice<Data> {
        get {
            return MutableRandomAccessSlice(base: self, bounds: bounds)
        }
        set {
            replaceSubrange(bounds, with: newValue.base)
        }
    }
    
    /// The start `Index` in the data.
    public var startIndex: Index {
        return 0
    }
    
    /// The end `Index` into the data.
    ///
    /// This is the "one-past-the-end" position, and will always be equal to the `count`.
    public var endIndex: Index {
        return count
    }
    
    public func index(before i: Index) -> Index {
        return i - 1
    }

    public func index(after i: Index) -> Index {
        return i + 1
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
            _idx = 0
            _endIdx = _data.endIndex
        }

        public mutating func next() -> UInt8? {
            guard _idx < _endIdx else { return nil }
            defer { _idx += 1 }
            let bufferSize = MemoryLayout.size(ofValue: _buffer)
            return withUnsafeMutablePointer(to: &_buffer) { ptr_ in
                let ptr = UnsafeMutableRawPointer(ptr_).assumingMemoryBound(to: UInt8.self)
                let bufferIdx = _idx % bufferSize
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
    public var length : Int {
        get { fatalError() }
        set { fatalError() }
    }
    
    @available(*, unavailable, message: "use withUnsafeBytes instead")
    public var bytes: UnsafeRawPointer { fatalError() }
    
    @available(*, unavailable, message: "use withUnsafeMutableBytes instead")
    public var mutableBytes: UnsafeMutableRawPointer { fatalError() }

    /// Returns `true` if the two `Data` arguments are equal.
    public static func ==(d1 : Data, d2 : Data) -> Bool {
        return d1._wrapped.isEqual(to: d2)
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
            children.append((label: "bytes", value: self[0..<nBytes].map { $0 }))
        }
        
        let m = Mirror(self, children:children, displayStyle: Mirror.DisplayStyle.struct)
        return m
    }
}

extension Data {
    @available(*, unavailable, renamed: "copyBytes(to:count:)")
    public func getBytes(_ buffer: UnsafeMutablePointer<Swift.Void>, length: Int) { }

    @available(*, unavailable, renamed: "copyBytes(to:from:)")
    public func getBytes(_ buffer: UnsafeMutablePointer<Swift.Void>, range: NSRange) { }
}

/// Provides bridging functionality for struct Data to class NSData and vice-versa.
extension Data : _ObjectiveCBridgeable {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSData {
        return unsafeBitCast(_wrapped, to: NSData.self)
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
        var result: Data? = nil
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension NSData : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as Data)
    }
}

/// A NSData subclass that uses Swift reference counting.
///
/// This subclass implements the API of NSData by holding an instance and forwarding all implementation to that object.
/// Since it uses Swift reference counting, we can do correct uniqueness checks even if we pass this instance back to Objective-C. In Objective-C, it looks like an instance of NSData.
extension _SwiftNSData {
    // Stubs
    // -----
    
    @objc(length)
    var length : Int {
        get {
            return _mapUnmanaged { $0.length }
        }
    }
    
    @objc(bytes)
    var bytes : UnsafeRawPointer {
        return _mapUnmanaged { $0.bytes }
    }
    
    @objc(subdataWithRange:)
    func subdata(with range: NSRange) -> Data {
        return _mapUnmanaged { $0.subdata(with: range) }
    }
    
    @objc(getBytes:length:)
    func getBytes(_ buffer: UnsafeMutableRawPointer, length: Int) {
        return _mapUnmanaged { $0.getBytes(buffer, length: length) }
    }
    
    @objc(getBytes:range:)
    func getBytes(_ buffer: UnsafeMutableRawPointer, range: NSRange) {
        return _mapUnmanaged { $0.getBytes(buffer, range: range) }
    }
    
    @objc(isEqualToData:)
    func isEqual(to other: Data) -> Bool {
        return _mapUnmanaged { return $0.isEqual(to: other) }
    }
    
    @objc(writeToURL:options:error:)
    func write(to url: URL, options: Data.WritingOptions) throws {
        return try _mapUnmanaged { try $0.write(to: url, options: options) }
    }
    
    @objc(rangeOfData:options:range:)
    func range(of data: Data, options: Data.SearchOptions, range: NSRange) -> NSRange {
        return _mapUnmanaged {
            $0.range(of: data, options: options, in: range)
        }
    }
    
    @objc(enumerateByteRangesUsingBlock:)
    func enumerateByteRanges(using block: (UnsafeRawPointer, NSRange, UnsafeMutablePointer<ObjCBool>) -> Void) {
        return _mapUnmanaged { $0.enumerateBytes(block) }
    }
    
    @objc(base64EncodedStringWithOptions:)
    func base64EncodedString(options: Data.Base64EncodingOptions) -> String {
        return _mapUnmanaged { $0.base64EncodedString(options: options) }
    }
    
    @objc(base64EncodedDataWithOptions:)
    func base64EncodedData(options: Data.Base64EncodingOptions) -> Data {
        return _mapUnmanaged { $0.base64EncodedData(options: options) }
    }
}
