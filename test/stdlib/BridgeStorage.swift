//===--- BridgeStorage.swift.gyb ------------------------------*- swift -*-===//
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
//
//  Bridged types are notionally single-word beasts that either store
//  an objc class or a native Swift class.  We'd like to be able to
//  distinguish these cases efficiently.
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Swift

//===--- Code mimics the stdlib without using spare pointer bits ----------===//
import SwiftShims

protocol BridgeStorage {
  associatedtype Native : AnyObject
  associatedtype ObjC : AnyObject

  init(native: Native, isFlagged: Bool)
  init(native: Native)
  init(objC: ObjC)

  mutating func isUniquelyReferencedNative() -> Bool
  mutating func isUniquelyReferencedUnflaggedNative() -> Bool
  var isNative: Bool {get}
  var isObjC: Bool {get}
  var nativeInstance: Native {get}
  var unflaggedNativeInstance: Native {get}
  var objCInstance: ObjC {get}
}

extension _BridgeStorage : BridgeStorage {}


//===----------------------------------------------------------------------===//
//===--- Testing code -----------------------------------------------------===//
//===----------------------------------------------------------------------===//
import StdlibUnittest
var allTests = TestSuite("DiscriminatedBridgeObject")

class C {
  deinit {
    print("bye C!")
  }
}
import Foundation

func isOSAtLeast(_ major: Int, _ minor: Int, patch: Int = 0) -> Bool {
  // isOperatingSystemAtLeastVersion() is unavailable on some OS versions.
  if #available(iOS 8.0, OSX 10.10, *) {
    let procInfo: AnyObject = ProcessInfo.processInfo
    return procInfo.isOperatingSystemAtLeast(
             OperatingSystemVersion(majorVersion: major, minorVersion: minor,
                                    patchVersion: patch))
  }

  return false
}

func expectTagged(_ s: NSString, _ expected: Bool) -> NSString {
#if arch(x86_64)
  let mask: UInt = 0x8000000000000001
#elseif arch(arm64)
  let mask: UInt = 0x8000000000000000
#else
  let mask: UInt = 0
#endif

  var osSupportsTaggedStrings: Bool
#if os(iOS)
  // NSTaggedPointerString is enabled starting in iOS 9.0.
  osSupportsTaggedStrings = isOSAtLeast(9,0)
#elseif os(tvOS) || os(watchOS)
  // NSTaggedPointerString is supported in all versions of TVOS and watchOS.
  osSupportsTaggedStrings = true
#elseif os(OSX)
  // NSTaggedPointerString is enabled starting in OS X 10.10.
  osSupportsTaggedStrings = isOSAtLeast(10,10)
#endif

  let taggedStringsSupported = osSupportsTaggedStrings && mask != 0

  let tagged = unsafeBitCast(s, to: UInt.self) & mask != 0

  if taggedStringsSupported  &&  expected == tagged {
    // okay
  } else if !taggedStringsSupported  &&  !tagged {
    // okay
  } else {
    let un = !tagged ? "un" : ""
    fatalError("Unexpectedly \(un)tagged pointer for string \"\(s)\"")
  }

  return s
}

var taggedNSString : NSString {
  return expectTagged(NSString(format: "foo"), true)
}

var unTaggedNSString : NSString {
  return expectTagged("fûtbōl" as NSString, false)
}

allTests.test("_BridgeStorage") {
  typealias B = _BridgeStorage<C, NSString>

  let oy: NSString = "oy"
  expectTrue(B(objC: oy).objCInstance == oy)

  for flag in [false, true] {
    do {
      var b = B(native: C(), isFlagged: flag)
      expectFalse(b.isObjC)
      expectTrue(b.isNative)
      expectEqual(!flag, b.isUnflaggedNative)
      expectTrue(b.isUniquelyReferencedNative())
      if !flag {
        expectTrue(b.isUniquelyReferencedUnflaggedNative())
      }
    }

    do {
      let c = C()
      var b = B(native: c, isFlagged: flag)
      expectFalse(b.isObjC)
      expectTrue(b.isNative)
      expectFalse(b.isUniquelyReferencedNative())
      expectEqual(!flag, b.isUnflaggedNative)
      expectTrue(b.nativeInstance === c)
      if !flag {
        expectTrue(b.unflaggedNativeInstance === c)
        expectFalse(b.isUniquelyReferencedUnflaggedNative())
      }
    }

  }

  var b = B(native: C(), isFlagged: false)
  expectTrue(b.isUniquelyReferencedNative())

  // Add a reference and verify that it's still native but no longer unique
  var c = b
  expectFalse(b.isUniquelyReferencedNative())
  _fixLifetime(c) // make sure c is not killed early

  let n = C()
  var bb = B(native: n)
  expectTrue(bb.nativeInstance === n)
  expectTrue(bb.isNative)
  expectTrue(bb.isUnflaggedNative)
  expectFalse(bb.isObjC)

  var d = B(objC: taggedNSString)
  expectFalse(d.isUniquelyReferencedNative())
  expectFalse(d.isNative)
  expectFalse(d.isUnflaggedNative)
  expectTrue(d.isObjC)

  d = B(objC: unTaggedNSString)
  expectFalse(d.isUniquelyReferencedNative())
  expectFalse(d.isNative)
  expectFalse(d.isUnflaggedNative)
  expectTrue(d.isObjC)

}

runAllTests()

