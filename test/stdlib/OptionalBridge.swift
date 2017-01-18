//===--- OptionalBridge.swift - Tests of Optional bridging ----------------===//
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

// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

let tests = TestSuite("OptionalBridge")

// Work around bugs in the type checker preventing casts back to optional.
func cast<T>(_ value: AnyObject, to: T.Type) -> T {
  return value as! T
}

// expectEqual() helpers for deeper-nested nullability than StdlibUnittest
// provides.
func expectEqual<T: Equatable>(_ x: T??, _ y: T??) {
  switch (x, y) {
  case (.some(let xx), .some(let yy)):
    expectEqual(xx, yy)
  case (.none, .none):
    return
  default:
    expectUnreachable("\(T.self)?? values don't match: \(x) vs. \(y)")
  }
}
func expectEqual<T: Equatable>(_ x: T???, _ y: T???) {
  switch (x, y) {
  case (.some(let xx), .some(let yy)):
    expectEqual(xx, yy)
  case (.none, .none):
    return
  default:
    expectUnreachable("\(T.self)??? values don't match: \(x) vs. \(y)")
  }
}

tests.test("wrapped value") {
  let unwrapped = "foo"
  let wrapped = Optional(unwrapped)
  let doubleWrapped = Optional(wrapped)

  let unwrappedBridged = unwrapped as AnyObject
  let wrappedBridged = wrapped as AnyObject
  let doubleWrappedBridged = doubleWrapped as AnyObject
  expectTrue(unwrappedBridged.isEqual(wrappedBridged)
             && wrappedBridged.isEqual(doubleWrappedBridged))

  let unwrappedCastBack = cast(unwrappedBridged, to: String.self)
  let wrappedCastBack = cast(wrappedBridged, to: Optional<String>.self)
  let doubleWrappedCastBack = cast(doubleWrappedBridged, to: Optional<String?>.self)

  expectEqual(unwrapped, unwrappedCastBack)
  expectEqual(wrapped, wrappedCastBack)
  expectEqual(doubleWrapped, doubleWrappedCastBack)
}

struct NotBridged: Hashable {
  var x: Int

  var hashValue: Int { return x }

  static func ==(x: NotBridged, y: NotBridged) -> Bool {
    return x.x == y.x
  }
}

tests.test("wrapped boxed value") {
  let unwrapped = NotBridged(x: 1738)
  let wrapped = Optional(unwrapped)
  let doubleWrapped = Optional(wrapped)

  let unwrappedBridged = unwrapped as AnyObject
  let wrappedBridged = wrapped as AnyObject
  let doubleWrappedBridged = doubleWrapped as AnyObject
  expectTrue(unwrappedBridged.isEqual(wrappedBridged))
  expectTrue(wrappedBridged.isEqual(doubleWrappedBridged))

  let unwrappedCastBack = cast(unwrappedBridged, to: NotBridged.self)
  let wrappedCastBack = cast(wrappedBridged, to: Optional<NotBridged>.self)
  let doubleWrappedCastBack = cast(doubleWrappedBridged, to: Optional<NotBridged?>.self)

  expectEqual(unwrapped, unwrappedCastBack)
  expectEqual(wrapped, wrappedCastBack)
  expectEqual(doubleWrapped, doubleWrappedCastBack)
}

tests.test("wrapped class instance") {
  let unwrapped = LifetimeTracked(0)
  let wrapped = Optional(unwrapped)

  expectTrue(wrapped as AnyObject === unwrapped as AnyObject)
}

tests.test("nil") {
  let null: String? = nil
  let wrappedNull = Optional(null)
  let doubleWrappedNull = Optional(wrappedNull)

  let nullBridged = null as AnyObject
  let wrappedNullBridged = wrappedNull as AnyObject
  let doubleWrappedNullBridged = doubleWrappedNull as AnyObject

  expectTrue(nullBridged === NSNull())
  expectTrue(wrappedNullBridged === NSNull())
  expectTrue(doubleWrappedNullBridged === NSNull())

  let nullCastBack = cast(nullBridged, to: Optional<String>.self)
  let wrappedNullCastBack = cast(nullBridged, to: Optional<String?>.self)
  let doubleWrappedNullCastBack = cast(nullBridged, to: Optional<String??>.self)

  expectEqual(nullCastBack, null)
  expectEqual(wrappedNullCastBack, wrappedNull)
  expectEqual(doubleWrappedNullCastBack, doubleWrappedNull)
}

tests.test("nil in nested optional") {
  let doubleNull: String?? = nil
  let wrappedDoubleNull = Optional(doubleNull)

  let doubleNullBridged = doubleNull as AnyObject
  let wrappedDoubleNullBridged = wrappedDoubleNull as AnyObject

  expectTrue(doubleNullBridged === wrappedDoubleNullBridged)
  expectTrue(doubleNullBridged !== NSNull())

  let doubleNullCastBack = cast(doubleNullBridged, to: Optional<String?>.self)
  let wrappedDoubleNullCastBack = cast(doubleNullBridged, to: Optional<String??>.self)

  expectEqual(doubleNullCastBack, doubleNull)
  expectEqual(wrappedDoubleNullCastBack, wrappedDoubleNull)

  let tripleNull: String??? = nil
  let tripleNullBridged = tripleNull as AnyObject

  expectTrue(doubleNullBridged !== tripleNullBridged)

  let tripleNullCastBack = cast(tripleNullBridged, to: Optional<String??>.self)
  expectEqual(tripleNullCastBack, tripleNull)
}

tests.test("collection of Optional") {
  let holeyArray: [LifetimeTracked?] = [LifetimeTracked(0), nil, LifetimeTracked(1)]
  let nsArray = holeyArray as NSArray

  autoreleasepool {
    expectTrue((nsArray[0] as AnyObject) === holeyArray[0]!)
    expectTrue((nsArray[1] as AnyObject) === NSNull())
    expectTrue((nsArray[2] as AnyObject) === holeyArray[2]!)
  }
}

tests.test("NSArray of NSNull") {
  let holeyNSArray: NSArray = [LifetimeTracked(2), NSNull(), LifetimeTracked(3)]
  autoreleasepool {
    let swiftArray = holeyNSArray as! [LifetimeTracked?]
    expectTrue(swiftArray[0]! === holeyNSArray[0] as AnyObject)
    expectTrue(swiftArray[1]  == nil)
    expectTrue(swiftArray[2]! === holeyNSArray[2] as AnyObject)
  }
}

runAllTests()
