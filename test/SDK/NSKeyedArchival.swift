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
import StdlibUnittest

// Since we test various apis with and without errors define these conveniences
func expectError(_ verb: () throws -> ()) {
  do {
    try verb()
    expectUnreachable("Expected to throw error!")
 } catch {
   // expected
  }
}
func expectNoError(_ verb: () throws -> ()) {
  do {
    try verb()
  } catch let error {
    expectUnreachable("Did not expect to throw error! \(error)")
  }
}

// Utilities for minimizing boiler plate in the tests
@available(OSX 10.11, iOS 9.0, *)
func archive(_ archival: (_: NSKeyedArchiver) throws -> Void) rethrows ->  Data {
    let data = NSMutableData()
    let ka = NSKeyedArchiver(forWritingWith: data)
    do {
      defer { ka.finishEncoding() }
      try archival(ka)
    }
    expectNotEqual(data.length, 0)
    return data as Data
}

@available(OSX 10.11, iOS 9.0, *)
func unarchive(_ data: Data, _ unarchival: (_: NSKeyedUnarchiver) throws -> Void) rethrows -> Void {
  let ku = NSKeyedUnarchiver(forReadingWith: data as Data)
  ku.requiresSecureCoding = true
  ku.decodingFailurePolicy = .setErrorAndReturn
  try unarchival(ku)
  ku.finishDecoding()
}


// MARK: - Data
// simple struct comprised of entirely plist types
struct Simple : Codable, Equatable {
    var number: Int = 5
    var string: String = "hello"
    var data: Data = Data(bytes: [0, 1, 2, 3])
    var array: [String] = ["stay", "a while", "and listen"]
    var dictionary: [String:String] = ["Deckard": "Cain"]
    static func ==(lhs: Simple, rhs: Simple) -> Bool {
        return lhs.number == rhs.number && lhs.string == rhs.string && lhs.data == rhs.data && lhs.array == rhs.array && lhs.dictionary == rhs.dictionary
    }
}

// simple struct comprised of entirely plist types
struct ThrowingCodable : Codable {
  public func encode(to encoder: Encoder) throws {
    throw NSError(domain: "sad", code: 3)
  }
}

// MARK - Tests
@available(OSX 10.11, iOS 9.0, *)
func test_simpleCodableSupport() {
  let s = Simple()
  
  var data: Data? = nil
  expectNoError {
    data = try archive { archiver in
      try archiver.encodeEncodable(s, forKey: "valid")
      try archiver.encodeEncodable(s, forKey: "wrong-type")
    }
  }
  
  unarchive(data!) { unarchiver in
    // confirm we can roundtrip our data
    let roundtrip = unarchiver.decodeDecodable(Simple.self, forKey: "valid")
    expectNotNil(roundtrip)
    if let rt = roundtrip {
        expectEqual(rt, s)
    } 
  
    // also ask for something that is not there
    let notThere = unarchiver.decodeDecodable(Simple.self, forKey: "not-there")
    expectNil(notThere)
    expectNil(unarchiver.error)
  
    // String != Simple so this should fail at the type level
    let wrongType = unarchiver.decodeDecodable(String.self, forKey: "wrong-type")
    expectNil(wrongType)
    expectNotNil(unarchiver.error)
  }
}

@available(OSX 10.11, iOS 9.0, *)
func test_encodableErrorHandling() {
  expectError {
    _ = try archive { archiver in
      try archiver.encodeEncodable(ThrowingCodable(), forKey: "non-codable")
    }
  }
}

@available(OSX 10.11, iOS 9.0, *)
func test_readingNonCodableFromDecodeDecodable() {
  var data: Data? = nil
  expectNoError {
    data = archive { archiver in
      archiver.encode(NSDate(), forKey: "non-codable")
    }
  }

  unarchive(data!) { unarchiver in
    let nonCodable = unarchiver.decodeDecodable(Simple.self, forKey: "non-codable")
    expectNil(nonCodable)
    expectNotNil(unarchiver.error)
  }
}

@available(OSX 10.11, iOS 9.0, *)
func test_toplevelAPIVariants() {
  let s = Simple()
  var data: Data? = nil
  expectNoError {
    data = try archive { archiver in
      try archiver.encodeEncodable(s, forKey: "valid")
      try archiver.encodeEncodable(Date(), forKey: "non-codable")
    }
  }

  unarchive(data!) { unarchiver in
    var caught = false
    do {
      _ = try unarchiver.decodeTopLevelDecodable(Simple.self, forKey: "non-codable")
    } catch {
      caught = true
    }
    expectTrue(caught)
  }

  expectNoError {
    _ = try unarchive(data!) { unarchiver in
      let roundtrip = try unarchiver.decodeTopLevelDecodable(Simple.self, forKey: "valid")
      expectNotNil(roundtrip)
      if let rt = roundtrip {
        expectEqual(rt, s)
      }
    }    
  }
}

@available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
func test_unarchiveObjectOfClass() {
  let topLevel = NSArray()
  let data = NSKeyedArchiver.archivedData(withRootObject: topLevel)

  // Should be able to decode an NSArray back out.
  expectNoError {
    guard let result = try NSKeyedUnarchiver.unarchivedObject(ofClass: NSArray.self, from: data) else {
      expectUnreachable("Unable to decode top-level array.")
      return
    }

    expectEqual(result, topLevel)
  }

  // Shouldn't be able to decode an NSString out of an NSArray.
  expectError {
    let _ = try NSKeyedUnarchiver.unarchivedObject(ofClass: NSString.self, from: data)
  }
}

@available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *)
func test_unarchiveObjectOfClasses() {
  let topLevel = NSArray()
  let data = NSKeyedArchiver.archivedData(withRootObject: topLevel)

  // Should be able to unarchive an array back out.
  expectNoError {
    guard let result = try NSKeyedUnarchiver.unarchivedObject(ofClasses: [NSArray.self], from: data) as? NSArray else {
      expectUnreachable("Unable to decode top-level array.")
      return
    }

    expectEqual(result, topLevel)
  }

  // Shouldn't be able to decode an NSString out of an NSArray.
  expectError {
    let _ = try NSKeyedUnarchiver.unarchivedObject(ofClasses: [NSString.self], from: data)
  }
}

// MARK: - Run Tests

#if !FOUNDATION_XCTEST
if #available(OSX 10.11, iOS 9.0, *) {
  let NSKeyedArchiverTest = TestSuite("TestNSKeyedArchiver")
  var tests = [
    "NSKeyedArchival.simpleCodableSupportInNSKeyedArchival": test_simpleCodableSupport,
    "NSKeyedArchival.encodableErrorHandling": test_encodableErrorHandling,
    "NSKeyedArchival.readingNonCodableFromDecodeDecodable": test_readingNonCodableFromDecodeDecodable,
    "NSKeyedArchival.toplevelAPIVariants": test_toplevelAPIVariants
  ]

  if #available(OSX 10.13, iOS 11.0, watchOS 4.0, tvOS 11.0, *) {
    tests["NSKeyedArchival.unarchiveObjectOfClass"] = test_unarchiveObjectOfClass
    tests["NSKeyedArchival.unarchiveObjectOfClasses"] = test_unarchiveObjectOfClasses
  }
  
  for (name, test) in tests {
    NSKeyedArchiverTest.test(name) { test() }
  }
  runAllTests()
}
#endif
