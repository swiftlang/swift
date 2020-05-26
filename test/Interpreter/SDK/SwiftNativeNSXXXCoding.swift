// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

let testSuite = TestSuite("SwiftNativeNSXXXCoding")

// Ensure that the class named `name`, subclass of T, gracefully handles
// being decoded. It doesn't have to work, just not crash.
private func test<T: NSObject & NSCoding>(type: T.Type, name: String) {
  if #available(macOS 10.13, iOS 11.0, tvOS 11.0, watchOS 4.0, *) {
    let archiver = NSKeyedArchiver(requiringSecureCoding: true)
    archiver.setClassName(name, for: T.self)
    archiver.encode(T(), forKey: "key")
    archiver.finishEncoding()
    let d = archiver.encodedData
  
    let unarchiver = try! NSKeyedUnarchiver(forReadingFrom: d)
    _ = unarchiver.decodeObject(of: T.self, forKey: "key")
  }
}
private func test<T: NSObject & NSCoding>(type: T.Type) {
  test(type: type, name: "__SwiftNative\(type)Base")
  test(type: type, name: "Swift.__SwiftNative\(type)")
}


// Test all the classes listed in SwiftNativeNSXXXBase.mm.gyb except for
// NSEnumerator (which doesn't conform to NSCoding).

if #available(iOS 13, macOS 10.15, tvOS 13, watchOS 6, *) {
  testSuite.test("NSArray") {
    test(type: NSArray.self)
  }

  testSuite.test("NSDictionary") {
    test(type: NSDictionary.self)
  }

  testSuite.test("NSSet") {
    test(type: NSSet.self)
  }

  testSuite.test("NSString") {
    test(type: NSString.self)
  }
}

runAllTests()
