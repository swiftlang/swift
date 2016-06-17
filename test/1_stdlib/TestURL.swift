// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
        
        expectTrue(url.pathComponents!.count > 0)
    }
    
    func testProperties() {
        let url = URL(fileURLWithPath: "/")
        do {
            let resourceValues = try url.resourceValues(forKeys: [.isVolumeKey, .nameKey])
            if let isVolume = resourceValues.isVolume {
                expectTrue(isVolume)
            }
            expectNotEmpty(resourceValues.name)
        } catch {
            expectTrue(false, "Should not have thrown")
        }
    }
    
    func testSetProperties() {
        // Create a temporary file
        var file = URL(fileURLWithPath: NSTemporaryDirectory())
        let name = "my_great_file" + UUID().uuidString
        try! file.appendPathComponent(name)
        let data = Data(bytes: [1, 2, 3, 4, 5])
        do {
            try data.write(to: file)
        } catch {
            expectTrue(false, "Unable to write data")
        }
        
        // Modify an existing resource values
        do {
            var resourceValues = try file.resourceValues(forKeys: [.nameKey])
            expectNotEmpty(resourceValues.name)
            expectEqual(resourceValues.name!, name)
            
            let newName = "goodbye cruel " + UUID().uuidString
            resourceValues.name = newName
            try file.setResourceValues(resourceValues)
        } catch {
            expectTrue(false, "Unable to set resources")
        }        
    }
    
    func testMoreSetProperties() {
        // Create a temporary file
        var file = URL(fileURLWithPath: NSTemporaryDirectory())
        let name = "my_great_file" + UUID().uuidString
        try! file.appendPathComponent(name)
        let data = Data(bytes: [1, 2, 3, 4, 5])
        do {
            try data.write(to: file)
        } catch {
            expectTrue(false, "Unable to write data")
        }

        do {
            var resourceValues = try file.resourceValues(forKeys: [.labelNumberKey])
            expectNotEmpty(resourceValues.labelNumber)
            
            // set label number
            resourceValues.labelNumber = 1
            try file.setResourceValues(resourceValues)
            
            // get label number
            try file.resourceValues(forKeys: [.labelNumberKey])
            expectNotEmpty(resourceValues.labelNumber)
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
            expectNotEmpty(resourceValues2.labelNumber)
            expectEqual(resourceValues2.labelNumber!, 2)
        } catch (let e as NSError) {
            expectTrue(false, "Unable to load or set resources \(e)")
        } catch {
            expectTrue(false, "Unable to load or set resources (mysterious error)")
        }
        
        do {
            try FileManager.`default`().removeItem(at: file)
        } catch {
            expectTrue(false, "Unable to remove file")
        }

    }
    
    func testURLComponents() {
        // Not meant to be a test of all URL components functionality, just some basic bridging stuff
        let s = "http://www.apple.com/us/search/ipad?src=globalnav"
        let components = URLComponents(string: s)!
        expectNotEmpty(components)
        
        expectNotEmpty(components.host)
        expectEqual("www.apple.com", components.host)
        
        
        if #available(OSX 10.11, iOS 9.0, *) {
            let rangeOfHost = components.rangeOfHost!
            expectNotEmpty(rangeOfHost)
            expectEqual(s[rangeOfHost], "www.apple.com")
        }
        
        if #available(OSX 10.10, iOS 8.0, *) {
            let qi = components.queryItems!
            expectNotEmpty(qi)
            
            expectEqual(1, qi.count)
            let first = qi[0]
            
            expectEqual("src", first.name)
            expectNotEmpty(first.value)
            expectEqual("globalnav", first.value)
        }
    }
}

#if !FOUNDATION_XCTEST
var URLTests = TestSuite("TestURL")
URLTests.test("testBasics") { TestURL().testBasics() }
URLTests.test("testProperties") { TestURL().testProperties() }
URLTests.test("testSetProperties") { TestURL().testSetProperties() }
URLTests.test("testMoreSetProperties") { TestURL().testMoreSetProperties() }
URLTests.test("testURLComponents") { TestURL().testURLComponents() }
runAllTests()
#endif
