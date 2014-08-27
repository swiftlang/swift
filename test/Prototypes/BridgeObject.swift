//===--- BridgeObject.swift - Tests for single-word discriminated object --===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Bridged types are notionally single-word beasts that either store
//  an objc class or a native Swift class.  We'd like to be able to
//  distinguish these cases efficiently, which is why we have
//  <rdar://problem/18125016>.  Until then, we can build something
//  similar upon AnyObject and a couple of runtime hacks.
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-stdlib-swift | FileCheck %s

import Swift

//===--- Code destined for stdlib -----------------------------------------===//

@asmname("_swift_usesNativeSwiftReferenceCounting_nonNull")
func _swift_usesNativeSwiftReferenceCounting_nonNull(_: UnsafePointer<Void>) -> Bool

@asmname("_swift_isUniquelyReferencedNative_nonNull")
func _swift_isUniquelyReferencedNative_nonNull(_: UnsafePointer<Void>) -> Bool

/// A type that can store any object, efficiently discriminate between
/// native Swift and ObjectiveC classes, and report detect
/// uniquely-referenced native Swift classes, for copy-on-write
/// optimization.
struct BridgeObject {
  init(_ object: AnyObject) {
    self.object = object
  }

  mutating func isUniquelyReferencedNativeObject() -> Bool {
    return _swift_isUniquelyReferencedNative_nonNull(address)
  }
  
  var isNativeObject : Bool {
    return _swift_usesNativeSwiftReferenceCounting_nonNull(address)
  }

  var address: UnsafePointer<Void> {
    return UnsafePointer(Builtin.bridgeToRawPointer(object))
  }
  
  let object: AnyObject
}

//===--- Testing code -----------------------------------------------------===//
// CHECK: testing...
println("testing...")

class C {
  deinit {
    println("bye C!")
  }
}
import Foundation

func expectTagged(s: NSString, expected: Bool) -> NSString {
#if arch(x86_64)
  let mask: UWord = 0x8000000000000001
#elseif arch(arm64)
  let mask: UWord = 0x8000000000000000
#else
  let mask: UWord = 0
  #endif
  // If this API is present, the OS also supports tagged pointers
  if (NSProcessInfo.processInfo() as AnyObject).operatingSystemVersion? != nil {
    if mask != 0 && (unsafeBitCast(s, UWord.self) & mask != 0) != expected {
      fatalError("Unexpectedly (un-)tagged pointer")
    }
  }
  return s
}

var taggedNSString : NSString {
  return expectTagged(NSString(format: "foo"), true)
}

var unTaggedNSString : NSString {
  return expectTagged("fûtbōl" as NSString, false)
}

if true {
  var b = BridgeObject(C())
  println(b.isUniquelyReferencedNativeObject()) // CHECK-NEXT: true
  println(b.isNativeObject)                     // CHECK-NEXT: true

  // Add a reference and verify that it's still native but no longer unique
  var c = b
  println(b.isUniquelyReferencedNativeObject()) // CHECK-NEXT: false
  println(b.isNativeObject)                     // CHECK-NEXT: true
  
  var d = BridgeObject(taggedNSString)
  println(d.isUniquelyReferencedNativeObject()) // CHECK-NEXT: false
  println(d.isNativeObject)                     // CHECK-NEXT: false

  d = BridgeObject(unTaggedNSString)
  println(d.isUniquelyReferencedNativeObject()) // CHECK-NEXT: false
  println(d.isNativeObject)                     // CHECK-NEXT: false
}
// CHECK-NEXT: bye C!

println("done.") // CHECK-NEXT: done
