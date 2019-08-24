// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation

#if FOUNDATION_XCTEST
import XCTest
class TestUUIDSuper : XCTestCase { }
#else
import StdlibUnittest
class TestUUIDSuper { }
#endif


class TestUUID : TestUUIDSuper {
    func test_NS_Equality() {
        let uuidA = NSUUID(uuidString: "E621E1F8-C36C-495A-93FC-0C247A3E6E5F")
        let uuidB = NSUUID(uuidString: "e621e1f8-c36c-495a-93fc-0c247a3e6e5f")
        let uuidC = NSUUID(uuidBytes: [0xe6,0x21,0xe1,0xf8,0xc3,0x6c,0x49,0x5a,0x93,0xfc,0x0c,0x24,0x7a,0x3e,0x6e,0x5f])
        let uuidD = NSUUID()
        
        expectEqual(uuidA, uuidB, "String case must not matter.")
        expectEqual(uuidA, uuidC, "A UUID initialized with a string must be equal to the same UUID initialized with its UnsafePointer<UInt8> equivalent representation.")
        expectNotEqual(uuidC, uuidD, "Two different UUIDs must not be equal.")
    }
    
    func test_Equality() {
        let uuidA = UUID(uuidString: "E621E1F8-C36C-495A-93FC-0C247A3E6E5F")
        let uuidB = UUID(uuidString: "e621e1f8-c36c-495a-93fc-0c247a3e6e5f")
        let uuidC = UUID(uuid: uuid_t(0xe6,0x21,0xe1,0xf8,0xc3,0x6c,0x49,0x5a,0x93,0xfc,0x0c,0x24,0x7a,0x3e,0x6e,0x5f))
        let uuidD = UUID()
        
        expectEqual(uuidA, uuidB, "String case must not matter.")
        expectEqual(uuidA, uuidC, "A UUID initialized with a string must be equal to the same UUID initialized with its UnsafePointer<UInt8> equivalent representation.")
        expectNotEqual(uuidC, uuidD, "Two different UUIDs must not be equal.")
    }
    
    func test_NS_InvalidUUID() {
        let uuid = NSUUID(uuidString: "Invalid UUID")
        expectNil(uuid, "The convenience initializer `init?(uuidString string:)` must return nil for an invalid UUID string.")
    }
    
    func test_InvalidUUID() {
        let uuid = UUID(uuidString: "Invalid UUID")
        expectNil(uuid, "The convenience initializer `init?(uuidString string:)` must return nil for an invalid UUID string.")
    }
    
    func test_NS_uuidString() {
        let uuid = NSUUID(uuidBytes: [0xe6,0x21,0xe1,0xf8,0xc3,0x6c,0x49,0x5a,0x93,0xfc,0x0c,0x24,0x7a,0x3e,0x6e,0x5f])
        expectEqual(uuid.uuidString, "E621E1F8-C36C-495A-93FC-0C247A3E6E5F")
    }
    
    func test_uuidString() {
        let uuid = UUID(uuid: uuid_t(0xe6,0x21,0xe1,0xf8,0xc3,0x6c,0x49,0x5a,0x93,0xfc,0x0c,0x24,0x7a,0x3e,0x6e,0x5f))
        expectEqual(uuid.uuidString, "E621E1F8-C36C-495A-93FC-0C247A3E6E5F")
    }
    
    func test_description() {
        let uuid = UUID()
        expectEqual(uuid.description, uuid.uuidString, "The description must be the same as the uuidString.")
    }

    func test_roundTrips() {
        let ref = NSUUID()
        let valFromRef = ref as UUID
        var bytes: [UInt8] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        let valFromBytes = bytes.withUnsafeMutableBufferPointer { buffer -> UUID in
            ref.getBytes(buffer.baseAddress!)
            return UUID(uuid: UnsafeRawPointer(buffer.baseAddress!).load(as: uuid_t.self))
        }
        let valFromStr = UUID(uuidString: ref.uuidString)
        expectEqual(ref.uuidString, valFromRef.uuidString)
        expectEqual(ref.uuidString, valFromBytes.uuidString)
        expectNotNil(valFromStr)
        expectEqual(ref.uuidString, valFromStr!.uuidString)
    }
    
    func test_hash() {
        guard #available(macOS 10.15, iOS 13, watchOS 6, tvOS 13, *) else { return }
        let values: [UUID] = [
            // This list takes a UUID and tweaks every byte while
            // leaving the version/variant intact.
            UUID(uuidString: "a53baa1c-b4f5-48db-9467-9786b76b256c")!,
            UUID(uuidString: "a63baa1c-b4f5-48db-9467-9786b76b256c")!,
            UUID(uuidString: "a53caa1c-b4f5-48db-9467-9786b76b256c")!,
            UUID(uuidString: "a53bab1c-b4f5-48db-9467-9786b76b256c")!,
            UUID(uuidString: "a53baa1d-b4f5-48db-9467-9786b76b256c")!,
            UUID(uuidString: "a53baa1c-b5f5-48db-9467-9786b76b256c")!,
            UUID(uuidString: "a53baa1c-b4f6-48db-9467-9786b76b256c")!,
            UUID(uuidString: "a53baa1c-b4f5-49db-9467-9786b76b256c")!,
            UUID(uuidString: "a53baa1c-b4f5-48dc-9467-9786b76b256c")!,
            UUID(uuidString: "a53baa1c-b4f5-48db-9567-9786b76b256c")!,
            UUID(uuidString: "a53baa1c-b4f5-48db-9468-9786b76b256c")!,
            UUID(uuidString: "a53baa1c-b4f5-48db-9467-9886b76b256c")!,
            UUID(uuidString: "a53baa1c-b4f5-48db-9467-9787b76b256c")!,
            UUID(uuidString: "a53baa1c-b4f5-48db-9467-9786b86b256c")!,
            UUID(uuidString: "a53baa1c-b4f5-48db-9467-9786b76c256c")!,
            UUID(uuidString: "a53baa1c-b4f5-48db-9467-9786b76b266c")!,
            UUID(uuidString: "a53baa1c-b4f5-48db-9467-9786b76b256d")!,
        ]
        checkHashable(values, equalityOracle: { $0 == $1 })
    }

    func test_AnyHashableContainingUUID() {
        let values: [UUID] = [
            UUID(uuidString: "e621e1f8-c36c-495a-93fc-0c247a3e6e5f")!,
            UUID(uuidString: "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")!,
            UUID(uuidString: "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(UUID.self, type(of: anyHashables[0].base))
        expectEqual(UUID.self, type(of: anyHashables[1].base))
        expectEqual(UUID.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSUUID() {
        let values: [NSUUID] = [
            NSUUID(uuidString: "e621e1f8-c36c-495a-93fc-0c247a3e6e5f")!,
            NSUUID(uuidString: "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")!,
            NSUUID(uuidString: "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(UUID.self, type(of: anyHashables[0].base))
        expectEqual(UUID.self, type(of: anyHashables[1].base))
        expectEqual(UUID.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }
}

#if !FOUNDATION_XCTEST
var UUIDTests = TestSuite("TestUUID")
UUIDTests.test("test_NS_Equality") { TestUUID().test_NS_Equality() }
UUIDTests.test("test_Equality") { TestUUID().test_Equality() }
UUIDTests.test("test_NS_InvalidUUID") { TestUUID().test_NS_InvalidUUID() }
UUIDTests.test("test_InvalidUUID") { TestUUID().test_InvalidUUID() }
UUIDTests.test("test_NS_uuidString") { TestUUID().test_NS_uuidString() }
UUIDTests.test("test_uuidString") { TestUUID().test_uuidString() }
UUIDTests.test("test_description") { TestUUID().test_description() }
UUIDTests.test("test_roundTrips") { TestUUID().test_roundTrips() }
UUIDTests.test("test_hash") { TestUUID().test_hash() }
UUIDTests.test("test_AnyHashableContainingUUID") { TestUUID().test_AnyHashableContainingUUID() }
UUIDTests.test("test_AnyHashableCreatedFromNSUUID") { TestUUID().test_AnyHashableCreatedFromNSUUID() }
runAllTests()
#endif

