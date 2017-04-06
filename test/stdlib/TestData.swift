// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang %S/Inputs/FoundationBridge/FoundationBridge.m -c -o %t/FoundationBridgeObjC.o -g
// RUN: %target-build-swift %s -I %S/Inputs/FoundationBridge/ -Xlinker %t/FoundationBridgeObjC.o -o %t/TestData

// RUN: %target-run %t/TestData
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import Dispatch
import ObjectiveC
import FoundationBridgeObjC

#if FOUNDATION_XCTEST
import XCTest
class TestDataSuper : XCTestCase { }
#else
import StdlibUnittest
class TestDataSuper { }
#endif

class TestData : TestDataSuper {

    // This is a type of Data that starts off as a storage of all 0x01s, but it only creates that buffer when needed. When mutated it converts into a more traditional data storage backed by a buffer.
    class AllOnesData : NSMutableData {
        
        private var _length : Int
        var _pointer : UnsafeMutableBufferPointer<UInt8>? {
            willSet {
                if let p = _pointer { free(p.baseAddress) }
            }
        }
        
        override init(length : Int) {
            _length = length
            super.init()
        }
        
        required init?(coder aDecoder: NSCoder) {
            // Not tested
            fatalError()
        }
        
        deinit {
            if let p = _pointer {
                free(p.baseAddress)
            }
        }
        
        override var length : Int {
            get {
                return _length
            }
            set {
                if let ptr = _pointer {
                    // Copy the data to our new length buffer
                    let newBuffer = malloc(newValue)!
                    if newValue <= _length {
                        memmove(newBuffer, ptr.baseAddress, newValue)
                    } else if newValue > _length {
                        memmove(newBuffer, ptr.baseAddress, _length)
                        memset(newBuffer + _length, 1, newValue - _length)
                    }
                    let bytePtr = newBuffer.bindMemory(to: UInt8.self, capacity: newValue)
                    _pointer = UnsafeMutableBufferPointer(start: bytePtr, count: newValue)
                } else {
                    _length = newValue
                }
            }
        }
        
        override var bytes : UnsafeRawPointer {
            if let d = _pointer {
                return UnsafeRawPointer(d.baseAddress!)
            } else {
                // Need to allocate the buffer now.
                // It doesn't matter if the buffer is uniquely referenced or not here.
                let buffer = malloc(length)
                memset(buffer, 1, length)
                let bytePtr = buffer!.bindMemory(to: UInt8.self, capacity: length)
                let result = UnsafeMutableBufferPointer(start: bytePtr, count: length)
                _pointer = result
                return UnsafeRawPointer(result.baseAddress!)
            }
        }
        
        override var mutableBytes: UnsafeMutableRawPointer {
            let newBufferLength = _length
            let newBuffer = malloc(newBufferLength)
            if let ptr = _pointer {
                // Copy the existing data to the new box, then return its pointer
                memmove(newBuffer, ptr.baseAddress, newBufferLength)
            } else {
                // Set new data to 1s
                memset(newBuffer, 1, newBufferLength)
            }
            let bytePtr = newBuffer!.bindMemory(to: UInt8.self, capacity: newBufferLength)
            let result = UnsafeMutableBufferPointer(start: bytePtr, count: newBufferLength)
            _pointer = result
            _length = newBufferLength
            return UnsafeMutableRawPointer(result.baseAddress!)
        }
        
        override func getBytes(_ buffer: UnsafeMutableRawPointer, length: Int) {
            if let d = _pointer {
                // Get the real data from the buffer
                memmove(buffer, d.baseAddress, length)
            } else {
                // A more efficient implementation of getBytes in the case where no one has asked for our backing bytes
                memset(buffer, 1, length)
            }
        }
    }

    // MARK: -
    
    // String of course has its own way to get data, but this way tests our own data struct
    func dataFrom(_ string : String) -> Data {
        // Create a Data out of those bytes
        return string.utf8CString.withUnsafeBufferPointer { (ptr) in
            ptr.baseAddress!.withMemoryRebound(to: UInt8.self, capacity: ptr.count) {
                // Subtract 1 so we don't get the null terminator byte. This matches NSString behavior.
                return Data(bytes: $0, count: ptr.count - 1)
            }
        }
    }
    
    // MARK: -
    
    func testBasicConstruction() {

        // Make sure that we were able to create some data
        let hello = dataFrom("hello")
        let helloLength = hello.count
        expectEqual(hello[0], 0x68, "Unexpected first byte")
        
        let world = dataFrom(" world")
        var helloWorld = hello
        world.withUnsafeBytes {
            helloWorld.append($0, count: world.count)
        }
                
        expectEqual(hello[0], 0x68, "First byte should not have changed")
        expectEqual(hello.count, helloLength, "Length of first data should not have changed")
        expectEqual(helloWorld.count, hello.count + world.count, "The total length should include both buffers")
    }
    
    func testInitializationWithArray() {
        let data = Data(bytes: [1, 2, 3])
        expectEqual(3, data.count)
        
        let data2 = Data(bytes: [1, 2, 3].filter { $0 >= 2 })
        expectEqual(2, data2.count)
        
        let data3 = Data(bytes: [1, 2, 3, 4, 5][1..<3])
        expectEqual(2, data3.count)
    }
    
    func testMutableData() {
        let hello = dataFrom("hello")
        let helloLength = hello.count
        expectEqual(hello[0], 0x68, "Unexpected first byte")

        // Double the length
        var mutatingHello = hello
        mutatingHello.count *= 2
        
        expectEqual(hello.count, helloLength, "The length of the initial data should not have changed")
        expectEqual(mutatingHello.count, helloLength * 2, "The length should have changed")
        
        // Get the underlying data for hello2
        mutatingHello.withUnsafeMutableBytes { (bytes : UnsafeMutablePointer<UInt8>) in
            expectEqual(bytes.pointee, 0x68, "First byte should be 0x68")
            
            // Mutate it
            bytes.pointee = 0x67
            expectEqual(bytes.pointee, 0x67, "First byte should be 0x67")
            expectEqual(mutatingHello[0], 0x67, "First byte accessed via other method should still be 0x67")
            
            // Verify that the first data is still correct
            expectEqual(hello[0], 0x68, "The first byte should still be 0x68")
        }
    }
    
    func testCustomData() {
        let length = 5
        let allOnesData = Data(referencing: AllOnesData(length: length))
        expectEqual(1, allOnesData[0], "First byte of all 1s data should be 1")
        
        // Double the length
        var allOnesCopyToMutate = allOnesData
        allOnesCopyToMutate.count = allOnesData.count * 2
        
        expectEqual(allOnesData.count, length, "The length of the initial data should not have changed")
        expectEqual(allOnesCopyToMutate.count, length * 2, "The length should have changed")
        
        // Force the second data to create its storage
        allOnesCopyToMutate.withUnsafeMutableBytes { (bytes : UnsafeMutablePointer<UInt8>) in
            expectEqual(bytes.pointee, 1, "First byte should be 1")
            
            // Mutate the second data
            bytes.pointee = 0
            expectEqual(bytes.pointee, 0, "First byte should be 0")
            expectEqual(allOnesCopyToMutate[0], 0, "First byte accessed via other method should still be 0")
            
            // Verify that the first data is still 1
            expectEqual(allOnesData[0], 1, "The first byte should still be 1")
        }
        
    }
    
    func testBridgingDefault() {
        let hello = dataFrom("hello")
        // Convert from struct Data to NSData
        if let s = NSString(data: hello, encoding: String.Encoding.utf8.rawValue) {
            expectTrue(s.isEqual(to: "hello"), "The strings should be equal")
        }
        
        // Convert from NSData to struct Data
        let goodbye = dataFrom("goodbye")
        if let resultingData = NSString(string: "goodbye").data(using: String.Encoding.utf8.rawValue) {
            expectEqual(resultingData[0], goodbye[0], "First byte should be equal")
        }
    }
    
    func testBridgingMutable() {
        // Create a mutable data
        var helloWorld = dataFrom("hello")
        helloWorld.append(dataFrom("world"))
        
        // Convert from struct Data to NSData
        if let s = NSString(data: helloWorld, encoding: String.Encoding.utf8.rawValue) {
            expectTrue(s.isEqual(to: "helloworld"), "The strings should be equal")
        }

    }
    
    func testBridgingCustom() {
        // Let's use an AllOnesData with some Objective-C code
        let allOnes = AllOnesData(length: 64)
        
        // Type-erased
        let data = Data(referencing: allOnes)
        
        // Create a home for our test data
        let dirPath = (NSTemporaryDirectory() as NSString).appendingPathComponent(NSUUID().uuidString)
        try! FileManager.default.createDirectory(atPath: dirPath, withIntermediateDirectories: true, attributes: nil)
        let filePath = (dirPath as NSString).appendingPathComponent("temp_file")
        guard FileManager.default.createFile(atPath: filePath, contents: nil, attributes: nil) else { expectTrue(false, "Unable to create temporary file"); return}
        guard let fh = FileHandle(forWritingAtPath: filePath) else { expectTrue(false, "Unable to open temporary file"); return }
        defer { try! FileManager.default.removeItem(atPath: dirPath) }
        
        // Now use this data with some Objective-C code that takes NSData arguments
        fh.write(data)
        
        // Get the data back
        do {
            let url = URL(fileURLWithPath: filePath)
            let readData = try Data.init(contentsOf: url)
            expectEqual(data.count, readData.count, "The length of the data is not the same")
        } catch {
            expectTrue(false, "Unable to read back data")
            return
        }
    }
    
    func testEquality() {
        let d1 = dataFrom("hello")
        let d2 = dataFrom("hello")
        
        // Use == explicitly here to make sure we're calling the right methods
        expectTrue(d1 == d2, "Data should be equal")
    }
    
    func testDataInSet() {
        let d1 = dataFrom("Hello")
        let d2 = dataFrom("Hello")
        let d3 = dataFrom("World")
        
        var s = Set<Data>()
        s.insert(d1)
        s.insert(d2)
        s.insert(d3)
        
        expectEqual(s.count, 2, "Expected only two entries in the Set")
    }
    
    func testReplaceSubrange() {
        var hello = dataFrom("Hello")
        let world = dataFrom("World")
        
        hello[0] = world[0]
        expectEqual(hello[0], world[0])
        
        var goodbyeWorld = dataFrom("Hello World")
        let goodbye = dataFrom("Goodbye")
        let expected = dataFrom("Goodbye World")
        
        goodbyeWorld.replaceSubrange(0..<5, with: goodbye)
        expectEqual(goodbyeWorld, expected)
    }
    
    func testReplaceSubrange2() {
        let hello = dataFrom("Hello")
        let world = dataFrom(" World")
        let goodbye = dataFrom("Goodbye")
        let expected = dataFrom("Goodbye World")
        
        var mutateMe = hello
        mutateMe.append(world)

        if let found = mutateMe.range(of: hello) {
            mutateMe.replaceSubrange(found, with: goodbye)
        }
        expectEqual(mutateMe, expected)
    }
    
    func testReplaceSubrange3() {
        // The expected result
        let expectedBytes : [UInt8] = [1, 2, 9, 10, 11, 12, 13]
        let expected = expectedBytes.withUnsafeBufferPointer {
            return Data(buffer: $0)
        }
        
        // The data we'll mutate
        let someBytes : [UInt8] = [1, 2, 3, 4, 5]
        var a = someBytes.withUnsafeBufferPointer {
            return Data(buffer: $0)
        }
        
        // The bytes we'll insert
        let b : [UInt8] = [9, 10, 11, 12, 13]
        b.withUnsafeBufferPointer {
            a.replaceSubrange(2..<5, with: $0)
        }
        expectEqual(expected, a)
    }
    
    func testReplaceSubrange4() {
        let expectedBytes : [UInt8] = [1, 2, 9, 10, 11, 12, 13]
        let expected = Data(bytes: expectedBytes)
        
        // The data we'll mutate
        let someBytes : [UInt8] = [1, 2, 3, 4, 5]
        var a = Data(bytes: someBytes)
        
        // The bytes we'll insert
        let b : [UInt8] = [9, 10, 11, 12, 13]
        a.replaceSubrange(2..<5, with: b)
        expectEqual(expected, a)
    }
    
    func testReplaceSubrange5() {
        var d = Data(bytes: [1, 2, 3])
        d.replaceSubrange(0..<0, with: [4])
        expectEqual(Data(bytes: [4, 1, 2, 3]), d)
        
        d.replaceSubrange(0..<4, with: [9])
        expectEqual(Data(bytes: [9]), d)
        
        d.replaceSubrange(0..<d.count, with: [])
        expectEqual(Data(), d)
        
        d.replaceSubrange(0..<0, with: [1, 2, 3, 4])
        expectEqual(Data(bytes: [1, 2, 3, 4]), d)
        
        d.replaceSubrange(1..<3, with: [9, 8])
        expectEqual(Data(bytes: [1, 9, 8, 4]), d)
        
        d.replaceSubrange(d.count..<d.count, with: [5])
        expectEqual(Data(bytes: [1, 9, 8, 4, 5]), d)
    }

    func testRange() {
        let helloWorld = dataFrom("Hello World")
        let goodbye = dataFrom("Goodbye")
        let hello = dataFrom("Hello")

        do {
            let found = helloWorld.range(of: goodbye)
            expectNil(found)
        }

        do {
            let found = helloWorld.range(of: goodbye, options: .anchored)
            expectNil(found)
        }

        do {
            let found = helloWorld.range(of: hello, in: 7..<helloWorld.count)
            expectNil(found)
        }
    }
    
    func testInsertData() {
        let hello = dataFrom("Hello")
        let world = dataFrom(" World")
        let expected = dataFrom("Hello World")
        var helloWorld = dataFrom("")
        
        helloWorld.replaceSubrange(0..<0, with: world)
        helloWorld.replaceSubrange(0..<0, with: hello)
        
        expectEqual(helloWorld, expected)
    }
    
    func testLoops() {
        let hello = dataFrom("Hello")
        var count = 0
        for _ in hello {
            count += 1
        }
        expectEqual(count, 5)        
    }
    
    func testGenericAlgorithms() {
        let hello = dataFrom("Hello World")
        
        let isCapital = { (byte : UInt8) in byte >= 65 && byte <= 90 }
        
        let allCaps = hello.filter(isCapital)
        expectEqual(allCaps.count, 2)
        
        let capCount = hello.reduce(0) { isCapital($1) ? $0 + 1 : $0 }
        expectEqual(capCount, 2)
        
        let allLower = hello.map { isCapital($0) ? $0 + 31 : $0 }
        expectEqual(allLower.count, hello.count)
    }
    
    func testCustomDeallocator() {
        var deallocatorCalled = false
        
        // Scope the data to a block to control lifecycle
        autoreleasepool {
            let buffer = malloc(16)!
            let bytePtr = buffer.bindMemory(to: UInt8.self, capacity: 16)
            var data = Data(bytesNoCopy: bytePtr, count: 16, deallocator: .custom({ (ptr, size) in
                deallocatorCalled = true
                free(UnsafeMutableRawPointer(ptr))
            }))
            // Use the data
            data[0] = 1
         }
        
        expectTrue(deallocatorCalled, "Custom deallocator was never called")
    }
    
    func testCopyBytes() {
        let c = 10
        let underlyingBuffer = malloc(c * MemoryLayout<UInt16>.stride)!
        let u16Ptr = underlyingBuffer.bindMemory(to: UInt16.self, capacity: c)
        let buffer = UnsafeMutableBufferPointer<UInt16>(start: u16Ptr, count: c)
        
        buffer[0] = 0
        buffer[1] = 0
        
        var data = Data(capacity: c * MemoryLayout<UInt16>.stride)
        data.resetBytes(in: 0..<c * MemoryLayout<UInt16>.stride)
        data[0] = 0xFF
        data[1] = 0xFF
        let copiedCount = data.copyBytes(to: buffer)
        expectEqual(copiedCount, c * MemoryLayout<UInt16>.stride)
        
        expectEqual(buffer[0], 0xFFFF)
        free(underlyingBuffer)
    }
    
    func testCopyBytes_undersized() {
        let a : [UInt8] = [1, 2, 3, 4, 5]
        var data = a.withUnsafeBufferPointer {
            return Data(buffer: $0)
        }
        let expectedSize = MemoryLayout<UInt8>.stride * a.count
        expectEqual(expectedSize, data.count)
        
        let underlyingBuffer = unsafeBitCast(malloc(expectedSize - 1)!, to: UnsafeMutablePointer<UInt8>.self)
        let buffer = UnsafeMutableBufferPointer(start: underlyingBuffer, count: expectedSize - 1)
        
        // We should only copy in enough bytes that can fit in the buffer
        let copiedCount = data.copyBytes(to: buffer)
        expectEqual(expectedSize - 1, copiedCount)
        
        var index = 0
        for v in a[0..<expectedSize-1] {
            expectEqual(v, buffer[index])
            index += 1
        }
        
        free(underlyingBuffer)
    }

    func testCopyBytes_oversized() {
        let a : [Int32] = [1, 0, 1, 0, 1]
        var data = a.withUnsafeBufferPointer {
            return Data(buffer: $0)
        }
        let expectedSize = MemoryLayout<Int32>.stride * a.count
        expectEqual(expectedSize, data.count)
        
        let underlyingBuffer = unsafeBitCast(malloc(expectedSize + 1)!, to: UnsafeMutablePointer<UInt8>.self)
        let buffer = UnsafeMutableBufferPointer(start: underlyingBuffer, count: expectedSize + 1)
        
        let copiedCount = data.copyBytes(to: buffer)
        expectEqual(expectedSize, copiedCount)

        free(underlyingBuffer)
    }

    func testCopyBytes_ranges() {
        
        do {
            // Equal sized buffer, data
            let a : [UInt8] = [1, 2, 3, 4, 5]
            var data = a.withUnsafeBufferPointer {
                return Data(buffer: $0)
            }
            
            let underlyingBuffer = unsafeBitCast(malloc(data.count)!, to: UnsafeMutablePointer<UInt8>.self)
            let buffer = UnsafeMutableBufferPointer(start: underlyingBuffer, count: data.count)
            
            var copiedCount : Int
            
            copiedCount = data.copyBytes(to: buffer, from: 0..<0)
            expectEqual(0, copiedCount)
            
            copiedCount = data.copyBytes(to: buffer, from: 1..<1)
            expectEqual(0, copiedCount)
            
            copiedCount = data.copyBytes(to: buffer, from: 0..<3)
            expectEqual((0..<3).count, copiedCount)
            
            var index = 0
            for v in a[0..<3] {
                expectEqual(v, buffer[index])
                index += 1
            }
            free(underlyingBuffer)
        }
        
        do {
            // Larger buffer than data
            let a : [UInt8] = [1, 2, 3, 4]
            let data = a.withUnsafeBufferPointer {
                return Data(buffer: $0)
            }
            
            let underlyingBuffer = unsafeBitCast(malloc(10)!, to: UnsafeMutablePointer<UInt8>.self)
            let buffer = UnsafeMutableBufferPointer(start: underlyingBuffer, count: 10)
            
            var copiedCount : Int
            
            copiedCount = data.copyBytes(to: buffer, from: 0..<3)
            expectEqual((0..<3).count, copiedCount)
            
            var index = 0
            for v in a[0..<3] {
                expectEqual(v, buffer[index])
                index += 1
            }
            free(underlyingBuffer)
        }
        
        do {
            // Larger data than buffer
            let a : [UInt8] = [1, 2, 3, 4, 5, 6]
            let data = a.withUnsafeBufferPointer {
                return Data(buffer: $0)
            }
            
            let underlyingBuffer = unsafeBitCast(malloc(4)!, to: UnsafeMutablePointer<UInt8>.self)
            let buffer = UnsafeMutableBufferPointer(start: underlyingBuffer, count: 4)
            
            var copiedCount : Int
            
            copiedCount = data.copyBytes(to: buffer, from: 0..<data.index(before: data.endIndex))
            expectEqual(4, copiedCount)
            
            var index = 0
            for v in a[0..<4] {
                expectEqual(v, buffer[index])
                index += 1
            }
            free(underlyingBuffer)

        }
    }

    func test_base64Data_small() {
        let data = "Hello World".data(using: .utf8)!
        let base64 = data.base64EncodedString()
        expectEqual("SGVsbG8gV29ybGQ=", base64, "trivial base64 conversion should work")
    }

    func test_dataHash() {
        let dataStruct = "Hello World".data(using: .utf8)!
        let dataObj = dataStruct as NSData
        expectEqual(dataObj.hashValue, dataStruct.hashValue, "Data and NSData should have the same hash value")
    }

    func test_base64Data_medium() {
        let data = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut at tincidunt arcu. Suspendisse nec sodales erat, sit amet imperdiet ipsum. Etiam sed ornare felis. Nunc mauris turpis, bibendum non lectus quis, malesuada placerat turpis. Nam adipiscing non massa et semper. Nulla convallis semper bibendum. Aliquam dictum nulla cursus mi ultricies, at tincidunt mi sagittis. Nulla faucibus at dui quis sodales. Morbi rutrum, dui id ultrices venenatis, arcu urna egestas felis, vel suscipit mauris arcu quis risus. Nunc venenatis ligula at orci tristique, et mattis purus pulvinar. Etiam ultricies est odio. Nunc eleifend malesuada justo, nec euismod sem ultrices quis. Etiam nec nibh sit amet lorem faucibus dapibus quis nec leo. Praesent sit amet mauris vel lacus hendrerit porta mollis consectetur mi. Donec eget tortor dui. Morbi imperdiet, arcu sit amet elementum interdum, quam nisl tempor quam, vitae feugiat augue purus sed lacus. In ac urna adipiscing purus venenatis volutpat vel et metus. Nullam nec auctor quam. Phasellus porttitor felis ac nibh gravida suscipit tempus at ante. Nunc pellentesque iaculis sapien a mattis. Aenean eleifend dolor non nunc laoreet, non dictum massa aliquam. Aenean quis turpis augue. Praesent augue lectus, mollis nec elementum eu, dignissim at velit. Ut congue neque id ullamcorper pellentesque. Maecenas euismod in elit eu vehicula. Nullam tristique dui nulla, nec convallis metus suscipit eget. Cras semper augue nec cursus blandit. Nulla rhoncus et odio quis blandit. Praesent lobortis dignissim velit ut pulvinar. Duis interdum quam adipiscing dolor semper semper. Nunc bibendum convallis dui, eget mollis magna hendrerit et. Morbi facilisis, augue eu fringilla convallis, mauris est cursus dolor, eu posuere odio nunc quis orci. Ut eu justo sem. Phasellus ut erat rhoncus, faucibus arcu vitae, vulputate erat. Aliquam nec magna viverra, interdum est vitae, rhoncus sapien. Duis tincidunt tempor ipsum ut dapibus. Nullam commodo varius metus, sed sollicitudin eros. Etiam nec odio et dui tempor blandit posuere.".data(using: .utf8)!
        let base64 = data.base64EncodedString()
        expectEqual("TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2NpbmcgZWxpdC4gVXQgYXQgdGluY2lkdW50IGFyY3UuIFN1c3BlbmRpc3NlIG5lYyBzb2RhbGVzIGVyYXQsIHNpdCBhbWV0IGltcGVyZGlldCBpcHN1bS4gRXRpYW0gc2VkIG9ybmFyZSBmZWxpcy4gTnVuYyBtYXVyaXMgdHVycGlzLCBiaWJlbmR1bSBub24gbGVjdHVzIHF1aXMsIG1hbGVzdWFkYSBwbGFjZXJhdCB0dXJwaXMuIE5hbSBhZGlwaXNjaW5nIG5vbiBtYXNzYSBldCBzZW1wZXIuIE51bGxhIGNvbnZhbGxpcyBzZW1wZXIgYmliZW5kdW0uIEFsaXF1YW0gZGljdHVtIG51bGxhIGN1cnN1cyBtaSB1bHRyaWNpZXMsIGF0IHRpbmNpZHVudCBtaSBzYWdpdHRpcy4gTnVsbGEgZmF1Y2lidXMgYXQgZHVpIHF1aXMgc29kYWxlcy4gTW9yYmkgcnV0cnVtLCBkdWkgaWQgdWx0cmljZXMgdmVuZW5hdGlzLCBhcmN1IHVybmEgZWdlc3RhcyBmZWxpcywgdmVsIHN1c2NpcGl0IG1hdXJpcyBhcmN1IHF1aXMgcmlzdXMuIE51bmMgdmVuZW5hdGlzIGxpZ3VsYSBhdCBvcmNpIHRyaXN0aXF1ZSwgZXQgbWF0dGlzIHB1cnVzIHB1bHZpbmFyLiBFdGlhbSB1bHRyaWNpZXMgZXN0IG9kaW8uIE51bmMgZWxlaWZlbmQgbWFsZXN1YWRhIGp1c3RvLCBuZWMgZXVpc21vZCBzZW0gdWx0cmljZXMgcXVpcy4gRXRpYW0gbmVjIG5pYmggc2l0IGFtZXQgbG9yZW0gZmF1Y2lidXMgZGFwaWJ1cyBxdWlzIG5lYyBsZW8uIFByYWVzZW50IHNpdCBhbWV0IG1hdXJpcyB2ZWwgbGFjdXMgaGVuZHJlcml0IHBvcnRhIG1vbGxpcyBjb25zZWN0ZXR1ciBtaS4gRG9uZWMgZWdldCB0b3J0b3IgZHVpLiBNb3JiaSBpbXBlcmRpZXQsIGFyY3Ugc2l0IGFtZXQgZWxlbWVudHVtIGludGVyZHVtLCBxdWFtIG5pc2wgdGVtcG9yIHF1YW0sIHZpdGFlIGZldWdpYXQgYXVndWUgcHVydXMgc2VkIGxhY3VzLiBJbiBhYyB1cm5hIGFkaXBpc2NpbmcgcHVydXMgdmVuZW5hdGlzIHZvbHV0cGF0IHZlbCBldCBtZXR1cy4gTnVsbGFtIG5lYyBhdWN0b3IgcXVhbS4gUGhhc2VsbHVzIHBvcnR0aXRvciBmZWxpcyBhYyBuaWJoIGdyYXZpZGEgc3VzY2lwaXQgdGVtcHVzIGF0IGFudGUuIE51bmMgcGVsbGVudGVzcXVlIGlhY3VsaXMgc2FwaWVuIGEgbWF0dGlzLiBBZW5lYW4gZWxlaWZlbmQgZG9sb3Igbm9uIG51bmMgbGFvcmVldCwgbm9uIGRpY3R1bSBtYXNzYSBhbGlxdWFtLiBBZW5lYW4gcXVpcyB0dXJwaXMgYXVndWUuIFByYWVzZW50IGF1Z3VlIGxlY3R1cywgbW9sbGlzIG5lYyBlbGVtZW50dW0gZXUsIGRpZ25pc3NpbSBhdCB2ZWxpdC4gVXQgY29uZ3VlIG5lcXVlIGlkIHVsbGFtY29ycGVyIHBlbGxlbnRlc3F1ZS4gTWFlY2VuYXMgZXVpc21vZCBpbiBlbGl0IGV1IHZlaGljdWxhLiBOdWxsYW0gdHJpc3RpcXVlIGR1aSBudWxsYSwgbmVjIGNvbnZhbGxpcyBtZXR1cyBzdXNjaXBpdCBlZ2V0LiBDcmFzIHNlbXBlciBhdWd1ZSBuZWMgY3Vyc3VzIGJsYW5kaXQuIE51bGxhIHJob25jdXMgZXQgb2RpbyBxdWlzIGJsYW5kaXQuIFByYWVzZW50IGxvYm9ydGlzIGRpZ25pc3NpbSB2ZWxpdCB1dCBwdWx2aW5hci4gRHVpcyBpbnRlcmR1bSBxdWFtIGFkaXBpc2NpbmcgZG9sb3Igc2VtcGVyIHNlbXBlci4gTnVuYyBiaWJlbmR1bSBjb252YWxsaXMgZHVpLCBlZ2V0IG1vbGxpcyBtYWduYSBoZW5kcmVyaXQgZXQuIE1vcmJpIGZhY2lsaXNpcywgYXVndWUgZXUgZnJpbmdpbGxhIGNvbnZhbGxpcywgbWF1cmlzIGVzdCBjdXJzdXMgZG9sb3IsIGV1IHBvc3VlcmUgb2RpbyBudW5jIHF1aXMgb3JjaS4gVXQgZXUganVzdG8gc2VtLiBQaGFzZWxsdXMgdXQgZXJhdCByaG9uY3VzLCBmYXVjaWJ1cyBhcmN1IHZpdGFlLCB2dWxwdXRhdGUgZXJhdC4gQWxpcXVhbSBuZWMgbWFnbmEgdml2ZXJyYSwgaW50ZXJkdW0gZXN0IHZpdGFlLCByaG9uY3VzIHNhcGllbi4gRHVpcyB0aW5jaWR1bnQgdGVtcG9yIGlwc3VtIHV0IGRhcGlidXMuIE51bGxhbSBjb21tb2RvIHZhcml1cyBtZXR1cywgc2VkIHNvbGxpY2l0dWRpbiBlcm9zLiBFdGlhbSBuZWMgb2RpbyBldCBkdWkgdGVtcG9yIGJsYW5kaXQgcG9zdWVyZS4=", base64, "medium base64 conversion should work")
    }

    func test_discontiguousEnumerateBytes() {
        let dataToEncode = "Hello World".data(using: .utf8)!

        let subdata1 = dataToEncode.withUnsafeBytes { bytes in
            return DispatchData(bytes: UnsafeBufferPointer(start: bytes, count: dataToEncode.count))
        }
        let subdata2 = dataToEncode.withUnsafeBytes { bytes in
            return DispatchData(bytes: UnsafeBufferPointer(start: bytes, count: dataToEncode.count))
        }
        var data = subdata1
        data.append(subdata2)

        var numChunks = 0
        data.enumerateBytes() { buffer, offset, stop in
            numChunks += 1
        }

        expectEqual(2, numChunks, "composing two dispatch_data should enumerate as structural data as 2 chunks")
    }

    func test_basicReadWrite() {
        let url = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true).appendingPathComponent("testfile")

        let count = 1 << 24
        let randomMemory = malloc(count)!
        let ptr = randomMemory.bindMemory(to: UInt8.self, capacity: count)
        let data = Data(bytesNoCopy: ptr, count: count, deallocator: .free)
        do {
            try data.write(to: url)
            let readData = try Data(contentsOf: url)
            expectEqual(data, readData)
        } catch {
            expectTrue(false, "Should not have thrown")
        }
        
        do {
            try FileManager.default.removeItem(at: url)
        } catch {
            // ignore
        }
    }

    func test_writeFailure() {
        let url = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true).appendingPathComponent("testfile")
        
        let data = Data()
        do {
            try data.write(to: url)
        } catch let error as NSError {
            print(error)
            expectTrue(false, "Should not have thrown")
        }
        
        do {
            try data.write(to: url, options: [.withoutOverwriting])
            expectTrue(false, "Should have thrown")
        } catch let error as NSError {
            expectEqual(error.code, NSFileWriteFileExistsError)
        }
        
        do {
            try FileManager.default.removeItem(at: url)
        } catch {
            // ignore
        }
        
        // Make sure clearing the error condition allows the write to succeed
        do {
            try data.write(to: url, options: [.withoutOverwriting])
        } catch {
            expectTrue(false, "Should not have thrown")
        }
        
        do {
            try FileManager.default.removeItem(at: url)
        } catch {
            // ignore
        }
    }
    
    func test_genericBuffers() {
        let a : [Int32] = [1, 0, 1, 0, 1]
        var data = a.withUnsafeBufferPointer {
            return Data(buffer: $0)
        }
        
        var expectedSize = MemoryLayout<Int32>.stride * a.count
        expectEqual(expectedSize, data.count)
        
        [false, true].withUnsafeBufferPointer {
            data.append($0)
        }
        
        expectedSize += MemoryLayout<Bool>.stride * 2
        expectEqual(expectedSize, data.count)
        
        let underlyingBuffer = unsafeBitCast(malloc(expectedSize)!, to: UnsafeMutablePointer<UInt8>.self)
        
        let buffer = UnsafeMutableBufferPointer(start: underlyingBuffer, count: expectedSize)
        let copiedCount = data.copyBytes(to: buffer)
        expectEqual(copiedCount, expectedSize)
        
        free(underlyingBuffer)
    }

    func test_basicDataMutation() {
        let object = ImmutableDataVerifier()

        object.verifier.reset()
        var data = object as Data
        expectTrue(object.verifier.wasCopied)
        expectFalse(object.verifier.wasMutableCopied)

        object.verifier.reset()
        expectTrue(data.count == object.length)
        expectFalse(object.verifier.wasCopied)

        object.verifier.reset()
        data.append("test", count: 4)
        expectTrue(object.verifier.wasMutableCopied)

        let preservedObjectness = (data as NSData) is MutableDataVerifier
        expectTrue(preservedObjectness)
    }

    func test_basicMutableDataMutation() {
        let object = MutableDataVerifier()
        
        object.verifier.reset()
        var data = object as Data
        expectTrue(object.verifier.wasCopied)
        expectFalse(object.verifier.wasMutableCopied)
        
        object.verifier.reset()
        expectTrue(data.count == object.length)
        expectFalse(object.verifier.wasCopied)
        
        object.verifier.reset()
        data.append("test", count: 4)
        expectTrue(object.verifier.wasMutableCopied)
        
        let preservedObjectness = (data as NSData) is MutableDataVerifier
        expectTrue(preservedObjectness)
    }

    func test_roundTrip() {
        let data = returnsData()
        expectTrue(identityOfData(data))
    }

    func test_passing() {
        let object = ImmutableDataVerifier()
        takesData(object as Data)
        expectTrue(object.verifier.wasCopied)
    }
    
    // intentionally structured so sizeof() != strideof()
    struct MyStruct {
        var time: UInt64
        let x: UInt32
        let y: UInt32
        let z: UInt32
        init() {
            time = 0
            x = 1
            y = 2
            z = 3
        }
    }
    
    func test_bufferSizeCalculation() {
        // Make sure that Data is correctly using strideof instead of sizeof.
        // n.b. if sizeof(MyStruct) == strideof(MyStruct), this test is not as useful as it could be
        
        // init
        let stuff = [MyStruct(), MyStruct(), MyStruct()]
        var data = stuff.withUnsafeBufferPointer {
            return Data(buffer: $0)
        }
        
        expectEqual(data.count, MemoryLayout<MyStruct>.stride * 3)
        
        
        // append
        stuff.withUnsafeBufferPointer {
            data.append($0)
        }
        
        expectEqual(data.count, MemoryLayout<MyStruct>.stride * 6)

        // copyBytes
        do {
            // equal size
            let underlyingBuffer = malloc(6 * MemoryLayout<MyStruct>.stride)!
            defer { free(underlyingBuffer) }

            let ptr = underlyingBuffer.bindMemory(to: MyStruct.self, capacity: 6)
            let buffer = UnsafeMutableBufferPointer<MyStruct>(start: ptr, count: 6)
            
            let byteCount = data.copyBytes(to: buffer)
            expectEqual(6 * MemoryLayout<MyStruct>.stride, byteCount)
        }
        
        do {
            // undersized
            let underlyingBuffer = malloc(3 * MemoryLayout<MyStruct>.stride)!
            defer { free(underlyingBuffer) }

            let ptr = underlyingBuffer.bindMemory(to: MyStruct.self, capacity: 3)
            let buffer = UnsafeMutableBufferPointer<MyStruct>(start: ptr, count: 3)
            
            let byteCount = data.copyBytes(to: buffer)
            expectEqual(3 * MemoryLayout<MyStruct>.stride, byteCount)
        }
        
        do {
            // oversized
            let underlyingBuffer = malloc(12 * MemoryLayout<MyStruct>.stride)!
            defer { free(underlyingBuffer) }
            
            let ptr = underlyingBuffer.bindMemory(to: MyStruct.self, capacity: 6)
            let buffer = UnsafeMutableBufferPointer<MyStruct>(start: ptr, count: 6)
            
            let byteCount = data.copyBytes(to: buffer)
            expectEqual(6 * MemoryLayout<MyStruct>.stride, byteCount)
        }
    }
    

    // MARK: -
    func test_classForCoder() {
        // confirm internal bridged impl types are not exposed to archival machinery
        let d = Data() as NSData
        let expected: AnyClass = NSData.self as AnyClass
        expectTrue(d.classForCoder == expected)
        expectTrue(d.classForKeyedArchiver == expected)
    }

    func test_AnyHashableContainingData() {
        let values: [Data] = [
            Data(base64Encoded: "AAAA")!,
            Data(base64Encoded: "AAAB")!,
            Data(base64Encoded: "AAAB")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Data.self, type(of: anyHashables[0].base))
        expectEqual(Data.self, type(of: anyHashables[1].base))
        expectEqual(Data.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSData() {
        let values: [NSData] = [
            NSData(base64Encoded: "AAAA")!,
            NSData(base64Encoded: "AAAB")!,
            NSData(base64Encoded: "AAAB")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(Data.self, type(of: anyHashables[0].base))
        expectEqual(Data.self, type(of: anyHashables[1].base))
        expectEqual(Data.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_noCopyBehavior() {
        let ptr = UnsafeMutableRawPointer(bitPattern: 0x1)!
        
        var deallocated = false
        autoreleasepool {
            let data = Data(bytesNoCopy: ptr, count: 1, deallocator: .custom({ (bytes, length) in
                deallocated = true
            }))
            expectFalse(deallocated)
            let equal = data.withUnsafeBytes { (bytes: UnsafePointer<UInt8>) -> Bool in
                return ptr == UnsafeMutableRawPointer(mutating: bytes)
            }
            
            expectTrue(equal)
        }
        
        expectTrue(deallocated)
    }

    func test_doubleDeallocation() {
        let data = "12345679".data(using: .utf8)!
        let len = data.withUnsafeBytes { (bytes: UnsafePointer<UInt8>) -> Int in
            let slice = Data(bytesNoCopy: UnsafeMutablePointer(mutating: bytes), count: 1, deallocator: .none)
            return slice.count
        }
        expectEqual(len, 1)
    }

    func test_repeatingValueInitialization() {
        var d = Data(repeating: 0x01, count: 3)
        let elements = repeatElement(UInt8(0x02), count: 3) // ensure we fall into the sequence case
        d.append(contentsOf: elements)

        expectEqual(d[0], 0x01)
        expectEqual(d[1], 0x01)
        expectEqual(d[2], 0x01)

        expectEqual(d[3], 0x02)
        expectEqual(d[4], 0x02)
        expectEqual(d[5], 0x02)
    }

    func test_rangeSlice() {
        var a: [UInt8] = [0, 1, 2, 3, 4, 5, 6, 7]
        var d = Data(bytes: a)
        for i in 0..<d.count {
            for j in i..<d.count {
                let slice = d[i..<j]
                if i == 1 && j == 2 {
                    print("here")
                    
                }
                expectEqual(slice.count, j - i, "where index range is \(i)..<\(j)")
                expectEqual(slice.map { $0 }, a[i..<j].map { $0 }, "where index range is \(i)..<\(j)")
                expectEqual(slice.startIndex, i, "where index range is \(i)..<\(j)")
                expectEqual(slice.endIndex, j, "where index range is \(i)..<\(j)")
                for n in slice.startIndex..<slice.endIndex {
                    let p = slice[n]
                    let q = a[n]
                    expectEqual(p, q, "where index range is \(i)..<\(j) at index \(n)")
                }
            }
        }
    }

    func test_rangeZoo() {
        let r1 = Range(0..<1)
        let r2 = CountableRange(0..<1)
        let r3 = ClosedRange(0..<1)
        let r4 = CountableClosedRange(0..<1)

        let data = Data(bytes: [8, 1, 2, 3, 4])
        let slice1: Data = data[r1]
        let slice2: Data = data[r2]
        let slice3: Data = data[r3]
        let slice4: Data = data[r4]
        expectEqual(slice1[0], 8)
        expectEqual(slice2[0], 8)
        expectEqual(slice3[0], 8)
        expectEqual(slice4[0], 8)
    }
}

#if !FOUNDATION_XCTEST
var DataTests = TestSuite("TestData")
DataTests.test("testBasicConstruction") { TestData().testBasicConstruction() }
DataTests.test("testInitializationWithArray") { TestData().testInitializationWithArray() }
DataTests.test("testMutableData") { TestData().testMutableData() }
DataTests.test("testCustomData") { TestData().testCustomData() }
DataTests.test("testBridgingDefault") { TestData().testBridgingDefault() }
DataTests.test("testBridgingMutable") { TestData().testBridgingMutable() }
DataTests.test("testBridgingCustom") { TestData().testBridgingCustom() }
DataTests.test("testEquality") { TestData().testEquality() }
DataTests.test("testDataInSet") { TestData().testDataInSet() }
DataTests.test("testReplaceSubrange") { TestData().testReplaceSubrange() }
DataTests.test("testReplaceSubrange2") { TestData().testReplaceSubrange2() }
DataTests.test("testReplaceSubrange3") { TestData().testReplaceSubrange3() }
DataTests.test("testReplaceSubrange4") { TestData().testReplaceSubrange4() }
DataTests.test("testReplaceSubrange5") { TestData().testReplaceSubrange5() }
DataTests.test("testRange") { TestData().testRange() }
DataTests.test("testInsertData") { TestData().testInsertData() }
DataTests.test("testLoops") { TestData().testLoops() }
DataTests.test("testGenericAlgorithms") { TestData().testGenericAlgorithms() }
DataTests.test("testCustomDeallocator") { TestData().testCustomDeallocator() }
DataTests.test("testCopyBytes") { TestData().testCopyBytes() }
DataTests.test("testCopyBytes_undersized") { TestData().testCopyBytes_undersized() }
DataTests.test("testCopyBytes_oversized") { TestData().testCopyBytes_oversized() }
DataTests.test("testCopyBytes_ranges") { TestData().testCopyBytes_ranges() }
DataTests.test("test_base64Data_small") { TestData().test_base64Data_small() }
DataTests.test("test_base64Data_medium") { TestData().test_base64Data_medium() }
DataTests.test("test_dataHash") { TestData().test_dataHash() }
DataTests.test("test_discontiguousEnumerateBytes") { TestData().test_discontiguousEnumerateBytes() }
DataTests.test("test_basicReadWrite") { TestData().test_basicReadWrite() }
DataTests.test("test_writeFailure") { TestData().test_writeFailure() }
DataTests.test("test_genericBuffers") { TestData().test_genericBuffers() }
DataTests.test("test_basicDataMutation") { TestData().test_basicDataMutation() }
DataTests.test("test_basicMutableDataMutation") { TestData().test_basicMutableDataMutation() }
DataTests.test("test_roundTrip") { TestData().test_roundTrip() }
DataTests.test("test_passing") { TestData().test_passing() }
DataTests.test("test_bufferSizeCalculation") { TestData().test_bufferSizeCalculation() }
DataTests.test("test_classForCoder") { TestData().test_classForCoder() }
DataTests.test("test_AnyHashableContainingData") { TestData().test_AnyHashableContainingData() }
DataTests.test("test_AnyHashableCreatedFromNSData") { TestData().test_AnyHashableCreatedFromNSData() }
DataTests.test("test_noCopyBehavior") { TestData().test_noCopyBehavior() }
DataTests.test("test_doubleDeallocation") { TestData().test_doubleDeallocation() }
DataTests.test("test_repeatingValueInitialization") { TestData().test_repeatingValueInitialization() }
DataTests.test("test_rangeZoo") { TestData().test_rangeZoo() }

// XCTest does not have a crash detection, whereas lit does
DataTests.test("bounding failure subdata") {
    let data = "Hello World".data(using: .utf8)!
    expectCrashLater()
    let c = data.subdata(in: 5..<200)
}

DataTests.test("bounding failure replace") {
    var data = "Hello World".data(using: .utf8)!
    expectCrashLater()
    data.replaceSubrange(5..<200, with: Data())
}

DataTests.test("bounding failure replace2") {
    var data = "a".data(using: .utf8)!
    var bytes : [UInt8] = [1, 2, 3]
    expectCrashLater()
    bytes.withUnsafeBufferPointer {
        // lowerBound ok, upperBound after end of data
        data.replaceSubrange(0..<2, with: $0)
    }
}

DataTests.test("bounding failure replace3") {
    var data = "a".data(using: .utf8)!
    var bytes : [UInt8] = [1, 2, 3]
    expectCrashLater()
    bytes.withUnsafeBufferPointer {
        // lowerBound is > length
        data.replaceSubrange(2..<4, with: $0)
    }
}

DataTests.test("bounding failure replace4") {
    var data = "a".data(using: .utf8)!
    var bytes : [UInt8] = [1, 2, 3]
    expectCrashLater()
    // lowerBound is > length
    data.replaceSubrange(2..<4, with: bytes)
}

DataTests.test("bounding failure reset range") {
    var data = "Hello World".data(using: .utf8)!
    expectCrashLater()
    data.resetBytes(in: 100..<200)
}

DataTests.test("bounding failure append bad length") {
    var data = "Hello World".data(using: .utf8)!
    expectCrashLater()
    data.append("hello", count: -2)
}

DataTests.test("bounding failure append absurd length") {
    var data = "Hello World".data(using: .utf8)!
    expectCrashLater()
    data.append("hello", count: Int.min)
}

DataTests.test("bounding failure subscript")
        .skip(.always("fails with resilient stdlib (rdar://problem/30560514)"))
        .code {
    var data = "Hello World".data(using: .utf8)!
    expectCrashLater()
    data[100] = 4
}


runAllTests()
#endif

