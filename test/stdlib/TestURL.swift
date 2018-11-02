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
class TestURLSuper : XCTestCase { }
#else
import StdlibUnittest
class TestURLSuper { }
#endif

class TestURL : TestURLSuper {
    
    func testBasics() {
        let url = URL(fileURLWithPath: NSTemporaryDirectory())
        
        expectTrue(url.pathComponents.count > 0)
    }
    
    func testProperties() {
        let url = URL(fileURLWithPath: "/")
        do {
            let resourceValues = try url.resourceValues(forKeys: [.isVolumeKey, .nameKey])
            if let isVolume = resourceValues.isVolume {
                expectTrue(isVolume)
            }
            expectNotNil(resourceValues.name)
        } catch {
            expectTrue(false, "Should not have thrown")
        }
    }
    
    func testSetProperties() {
        // Create a temporary file
        var file = URL(fileURLWithPath: NSTemporaryDirectory())
        let name = "my_great_file" + UUID().uuidString
        file.appendPathComponent(name)
        let data = Data(bytes: [1, 2, 3, 4, 5])
        do {
            try data.write(to: file)
        } catch {
            expectTrue(false, "Unable to write data")
        }
        
        // Modify an existing resource value
        do {
            var resourceValues = try file.resourceValues(forKeys: [.nameKey])
            expectNotNil(resourceValues.name)
            expectEqual(resourceValues.name!, name)
            
            let newName = "goodbye cruel " + UUID().uuidString
            resourceValues.name = newName
            try file.setResourceValues(resourceValues)
        } catch {
            expectTrue(false, "Unable to set resources")
        }
    }
    
#if os(macOS)
    func testQuarantineProperties() {
        // Test the quarantine stuff; it has special logic
        if #available(OSX 10.11, iOS 9.0, *) {
            // Create a temporary file
            var file = URL(fileURLWithPath: NSTemporaryDirectory())
            let name = "my_great_file" + UUID().uuidString
            file.appendPathComponent(name)
            let data = Data(bytes: [1, 2, 3, 4, 5])
            do {
                try data.write(to: file)
            } catch {
                expectTrue(false, "Unable to write data")
            }

            // Set the quarantine info on a file
            do {
                var resourceValues = URLResourceValues()
                resourceValues.quarantineProperties = ["LSQuarantineAgentName" : "TestURL"]
                try file.setResourceValues(resourceValues)
            } catch {
                expectTrue(false, "Unable to set quarantine info")
            }
            
            // Get the quarantine info back
            do {
                var resourceValues = try file.resourceValues(forKeys: [.quarantinePropertiesKey])
                expectEqual(resourceValues.quarantineProperties?["LSQuarantineAgentName"] as? String, "TestURL")
            } catch {
                expectTrue(false, "Unable to get quarantine info")
            }
            
            // Clear the quarantine info
            do {
                var resourceValues = URLResourceValues()
                resourceValues.quarantineProperties = nil // this effectively sets a flag
                try file.setResourceValues(resourceValues)
                
                // Make sure that the resourceValues property returns nil
                expectNil(resourceValues.quarantineProperties)
            } catch {
                expectTrue(false, "Unable to clear quarantine info")
            }

            // Get the quarantine info back again
            do {
                var resourceValues = try file.resourceValues(forKeys: [.quarantinePropertiesKey])
                expectNil(resourceValues.quarantineProperties)
            } catch {
                expectTrue(false, "Unable to get quarantine info after clearing")
            }

        }
    }
#endif
    
    func testMoreSetProperties() {
        // Create a temporary file
        var file = URL(fileURLWithPath: NSTemporaryDirectory())
        let name = "my_great_file" + UUID().uuidString
        file.appendPathComponent(name)
        let data = Data(bytes: [1, 2, 3, 4, 5])
        do {
            try data.write(to: file)
        } catch {
            expectTrue(false, "Unable to write data")
        }

        do {
            var resourceValues = try file.resourceValues(forKeys: [.labelNumberKey])
            expectNotNil(resourceValues.labelNumber)
            
            // set label number
            resourceValues.labelNumber = 1
            try file.setResourceValues(resourceValues)
            
            // get label number
            let _ = try file.resourceValues(forKeys: [.labelNumberKey])
            expectNotNil(resourceValues.labelNumber)
            expectEqual(resourceValues.labelNumber!, 1)
        } catch (let e as NSError) {
            expectTrue(false, "Unable to load or set resources \(e)")
        } catch {
            expectTrue(false, "Unable to load or set resources (mysterious error)")
        }
        
        // Construct values from scratch
        do {
            var resourceValues = URLResourceValues()
            resourceValues.labelNumber = 2
            
            try file.setResourceValues(resourceValues)
            let resourceValues2 = try file.resourceValues(forKeys: [.labelNumberKey])
            expectNotNil(resourceValues2.labelNumber)
            expectEqual(resourceValues2.labelNumber!, 2)
        } catch (let e as NSError) {
            expectTrue(false, "Unable to load or set resources \(e)")
        } catch {
            expectTrue(false, "Unable to load or set resources (mysterious error)")
        }
        
        do {
            try FileManager.default.removeItem(at: file)
        } catch {
            expectTrue(false, "Unable to remove file")
        }

    }
    
    func testURLComponents() {
        // Not meant to be a test of all URL components functionality, just some basic bridging stuff
        let s = "http://www.apple.com/us/search/ipad?src=globalnav"
        let components = URLComponents(string: s)!
        expectNotNil(components)
        
        expectNotNil(components.host)
        expectEqual("www.apple.com", components.host)
        
        
        if #available(OSX 10.11, iOS 9.0, *) {
            let rangeOfHost = components.rangeOfHost!
            expectNotNil(rangeOfHost)
            expectEqual(s[rangeOfHost], "www.apple.com")
        }
        
        if #available(OSX 10.10, iOS 8.0, *) {
            let qi = components.queryItems!
            expectNotNil(qi)
            
            expectEqual(1, qi.count)
            let first = qi[0]
            
            expectEqual("src", first.name)
            expectNotNil(first.value)
            expectEqual("globalnav", first.value)
        }
    }
    
    func testURLResourceValues() {
        
        let fileName = "temp_file"
        var dir = URL(fileURLWithPath: NSTemporaryDirectory())
        dir.appendPathComponent(UUID().uuidString)
        
        try! FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true, attributes: nil)
        
        dir.appendPathComponent(fileName)
        try! Data(bytes: [1,2,3,4]).write(to: dir)
        
        defer {
            do {
                try FileManager.default.removeItem(at: dir)
            } catch {
                // Oh well
            }
        }
        
        do {
            let values = try dir.resourceValues(forKeys: [.nameKey, .isDirectoryKey])
            expectEqual(values.name, fileName)
            expectFalse(values.isDirectory!)
            expectEqual(nil, values.creationDate) // Didn't ask for this
        } catch {
            expectTrue(false, "Unable to get resource value")
        }
        
        let originalDate : Date
        do {
            var values = try dir.resourceValues(forKeys: [.creationDateKey])
            expectNotEqual(nil, values.creationDate)
            originalDate = values.creationDate!
        } catch {
            originalDate = Date()
            expectTrue(false, "Unable to get creation date")
        }
        
        let newDate = originalDate + 100
        
        do {
            var values = URLResourceValues()
            values.creationDate = newDate
            try dir.setResourceValues(values)
        } catch {
            expectTrue(false, "Unable to set resource value")
        }
        
        do {
            let values = try dir.resourceValues(forKeys: [.creationDateKey])
            expectEqual(newDate, values.creationDate)
        } catch {
            expectTrue(false, "Unable to get values")
        }
    }

    func test_AnyHashableContainingURL() {
        let values: [URL] = [
            URL(string: "https://example.com/")!,
            URL(string: "https://example.org/")!,
            URL(string: "https://example.org/")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(URL.self, type(of: anyHashables[0].base))
        expectEqual(URL.self, type(of: anyHashables[1].base))
        expectEqual(URL.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSURL() {
        let values: [NSURL] = [
            NSURL(string: "https://example.com/")!,
            NSURL(string: "https://example.org/")!,
            NSURL(string: "https://example.org/")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(URL.self, type(of: anyHashables[0].base))
        expectEqual(URL.self, type(of: anyHashables[1].base))
        expectEqual(URL.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableContainingURLComponents() {
        let values: [URLComponents] = [
            URLComponents(string: "https://example.com/")!,
            URLComponents(string: "https://example.org/")!,
            URLComponents(string: "https://example.org/")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(URLComponents.self, type(of: anyHashables[0].base))
        expectEqual(URLComponents.self, type(of: anyHashables[1].base))
        expectEqual(URLComponents.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSURLComponents() {
        let values: [NSURLComponents] = [
            NSURLComponents(string: "https://example.com/")!,
            NSURLComponents(string: "https://example.org/")!,
            NSURLComponents(string: "https://example.org/")!,
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(URLComponents.self, type(of: anyHashables[0].base))
        expectEqual(URLComponents.self, type(of: anyHashables[1].base))
        expectEqual(URLComponents.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableContainingURLQueryItem() {
        if #available(OSX 10.10, iOS 8.0, *) {
            let values: [URLQueryItem] = [
                URLQueryItem(name: "foo", value: nil),
                URLQueryItem(name: "bar", value: nil),
                URLQueryItem(name: "bar", value: nil),
            ]
            let anyHashables = values.map(AnyHashable.init)
            expectEqual(URLQueryItem.self, type(of: anyHashables[0].base))
            expectEqual(URLQueryItem.self, type(of: anyHashables[1].base))
            expectEqual(URLQueryItem.self, type(of: anyHashables[2].base))
            expectNotEqual(anyHashables[0], anyHashables[1])
            expectEqual(anyHashables[1], anyHashables[2])
        }
    }

    func test_AnyHashableCreatedFromNSURLQueryItem() {
        if #available(OSX 10.10, iOS 8.0, *) {
            let values: [NSURLQueryItem] = [
                NSURLQueryItem(name: "foo", value: nil),
                NSURLQueryItem(name: "bar", value: nil),
                NSURLQueryItem(name: "bar", value: nil),
            ]
            let anyHashables = values.map(AnyHashable.init)
            expectEqual(URLQueryItem.self, type(of: anyHashables[0].base))
            expectEqual(URLQueryItem.self, type(of: anyHashables[1].base))
            expectEqual(URLQueryItem.self, type(of: anyHashables[2].base))
            expectNotEqual(anyHashables[0], anyHashables[1])
            expectEqual(anyHashables[1], anyHashables[2])
        }
    }

    func test_AnyHashableContainingURLRequest() {
        let values: [URLRequest] = [
            URLRequest(url: URL(string: "https://example.com/")!),
            URLRequest(url: URL(string: "https://example.org/")!),
            URLRequest(url: URL(string: "https://example.org/")!),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(URLRequest.self, type(of: anyHashables[0].base))
        expectEqual(URLRequest.self, type(of: anyHashables[1].base))
        expectEqual(URLRequest.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }

    func test_AnyHashableCreatedFromNSURLRequest() {
        let values: [NSURLRequest] = [
            NSURLRequest(url: URL(string: "https://example.com/")!),
            NSURLRequest(url: URL(string: "https://example.org/")!),
            NSURLRequest(url: URL(string: "https://example.org/")!),
        ]
        let anyHashables = values.map(AnyHashable.init)
        expectEqual(URLRequest.self, type(of: anyHashables[0].base))
        expectEqual(URLRequest.self, type(of: anyHashables[1].base))
        expectEqual(URLRequest.self, type(of: anyHashables[2].base))
        expectNotEqual(anyHashables[0], anyHashables[1])
        expectEqual(anyHashables[1], anyHashables[2])
    }
}

#if !FOUNDATION_XCTEST
var URLTests = TestSuite("TestURL")
URLTests.test("testBasics") { TestURL().testBasics() }
URLTests.test("testProperties") { TestURL().testProperties() }
URLTests.test("testSetProperties") { TestURL().testSetProperties() }
#if os(macOS)
URLTests.test("testQuarantineProperties") { TestURL().testQuarantineProperties() }
#endif
URLTests.test("testMoreSetProperties") { TestURL().testMoreSetProperties() }
URLTests.test("testURLComponents") { TestURL().testURLComponents() }
URLTests.test("testURLResourceValues") { TestURL().testURLResourceValues() }
URLTests.test("test_AnyHashableContainingURL") { TestURL().test_AnyHashableContainingURL() }
URLTests.test("test_AnyHashableCreatedFromNSURL") { TestURL().test_AnyHashableCreatedFromNSURL() }
URLTests.test("test_AnyHashableContainingURLComponents") { TestURL().test_AnyHashableContainingURLComponents() }
URLTests.test("test_AnyHashableCreatedFromNSURLComponents") { TestURL().test_AnyHashableCreatedFromNSURLComponents() }
URLTests.test("test_AnyHashableContainingURLQueryItem") { TestURL().test_AnyHashableContainingURLQueryItem() }
URLTests.test("test_AnyHashableCreatedFromNSURLQueryItem") { TestURL().test_AnyHashableCreatedFromNSURLQueryItem() }
URLTests.test("test_AnyHashableContainingURLRequest") { TestURL().test_AnyHashableContainingURLRequest() }
URLTests.test("test_AnyHashableCreatedFromNSURLRequest") { TestURL().test_AnyHashableCreatedFromNSURLRequest() }
runAllTests()
#endif
