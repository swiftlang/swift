// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop
// REQUIRES: OS=macosx

import Foundation
import StdlibUnittest

protocol Fooable {
  func foo() -> String
}

func fooify<T>(_ x: T) -> String {
  if let foo = x as? Fooable {
    return foo.foo()
  } else {
    return "not fooable"
  }
}

extension NSRect: Fooable {
  func foo() -> String { return "NSRect" }
}

extension CFSet: Fooable {
  func foo() -> String { return "CFSet" }
}

extension NSString: Fooable {
  func foo() -> String { return "NSString" }
}

var ProtocolLookupForeign = TestSuite("ProtocolLookupForeign")

ProtocolLookupForeign.test("NSRect") {
  expectEqual("NSRect", fooify(NSRect()))
}

ProtocolLookupForeign.test("NSPoint") {
  expectEqual("not fooable", fooify(NSPoint()))
}

ProtocolLookupForeign.test("CFSet") {
  if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
    expectEqual("CFSet", fooify(CFSetCreate(kCFAllocatorDefault, nil, 0, nil)!))
  }
}

ProtocolLookupForeign.test("CFArray") {
  expectEqual("not fooable", fooify(CFArrayCreate(kCFAllocatorDefault, nil, 0, nil)!))
}

ProtocolLookupForeign.test("NSString") {
  expectEqual("NSString", fooify(NSString()))
}

ProtocolLookupForeign.test("NSMutableString") {
  expectEqual("NSString", fooify(NSMutableString()))
}

ProtocolLookupForeign.test("NSSet") {
  expectEqual("not fooable", fooify(NSSet()))
}

runAllTests()
