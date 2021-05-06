// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s
// REQUIRES: objc_interop

import Foundation

@objc protocol P {
  @objc optional static func test(a: UnsafePointer<Int>, _: Int)
  // expected-note@-1 {{candidate has partially matching parameter list (a: UnsafePointer<Int>, Int)}}
  @objc optional static func test(b: [Int], _: Int)
  // expected-note@-1 {{candidate has partially matching parameter list (b: [Int], Int)}}
}

@objc class S : NSObject, P {
  static func test(a: UnsafePointer<Int>, _: Int) {}
  static func test(b: [Int], _: Int) {}
}

func test(s: P.Type, v: Int) {
  _ = s.test!(c: v, 0) // expected-error {{no exact matches in call to static method 'test'}}
}
