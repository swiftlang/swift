// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library -o %t/libGetImageNameHelper.dylib -emit-module %S/Inputs/class_getImageName-helper.swift
// RUN: %target-codesign %t/libGetImageNameHelper.dylib

// RUN: %target-build-swift -g %s -I %t -o %t/main -L %t -lGetImageNameHelper
// RUN: %target-run %t/main %t/libGetImageNameHelper.dylib

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Darwin
import ObjectiveC
import GetImageNameHelper
import StdlibUnittest

func check(_ cls: AnyClass, in library: String) {
  guard let imageName = class_getImageName(cls) else {
    expectUnreachable("could not find image for \(cls)")
    return
  }
  expectTrue(String(cString: imageName).hasSuffix(library),
             "wrong library for \(cls)")
}

let isMissingObjCRuntimeHook =
    (nil == dlsym(UnsafeMutableRawPointer(bitPattern: -2),
                  "objc_setHook_getImageName"))

var testSuite = TestSuite("class_getImageName")

testSuite.test("Simple") {
  check(SimpleSwiftObject.self, in: "libGetImageNameHelper.dylib")
  check(SimpleNSObject.self, in: "libGetImageNameHelper.dylib")
}

testSuite.test("Generic")
    .xfail(.custom({ isMissingObjCRuntimeHook },
                   reason: "hook for class_getImageName not present"))
    .code {
  check(GenericSwiftObject<Int>.self, in: "libGetImageNameHelper.dylib")
  check(GenericSwiftObject<NSObject>.self, in: "libGetImageNameHelper.dylib")

  check(GenericNSObject<Int>.self, in: "libGetImageNameHelper.dylib")
  check(GenericNSObject<NSObject>.self, in: "libGetImageNameHelper.dylib")
}

testSuite.test("GenericAncestry")
    .xfail(.custom({ isMissingObjCRuntimeHook },
                   reason: "hook for class_getImageName not present"))
    .code {
  check(GenericAncestrySwiftObject.self, in: "libGetImageNameHelper.dylib")
  check(GenericAncestryNSObject.self, in: "libGetImageNameHelper.dylib")
}

testSuite.test("Resilient") {
  check(ResilientFieldSwiftObject.self, in: "libGetImageNameHelper.dylib")
  check(ResilientFieldNSObject.self, in: "libGetImageNameHelper.dylib")
}

testSuite.test("ObjC") {
  check(NSObject.self, in: "libobjc.A.dylib")
}

testSuite.test("KVO/Simple") {
  // We use object_getClass in this test to not look through KVO's artificial
  // subclass.
  let obj = SimpleNSObject()
  let observation = obj.observe(\.observableName) { _, _ in }
  withExtendedLifetime(observation) {
    let theClass = object_getClass(obj)
    precondition(theClass !== SimpleNSObject.self, "no KVO subclass?")
    expectNil(class_getImageName(theClass),
              "should match what happens with NSObject (below)")
  }
}

testSuite.test("KVO/GenericAncestry") {
  // We use object_getClass in this test to not look through KVO's artificial
  // subclass.
  let obj = GenericAncestryNSObject()
  let observation = obj.observe(\.observableName) { _, _ in }
  withExtendedLifetime(observation) {
    let theClass = object_getClass(obj)
    precondition(theClass !== GenericAncestryNSObject.self, "no KVO subclass?")
    expectNil(class_getImageName(theClass),
              "should match what happens with NSObject (below)")
  }
}

testSuite.test("KVO/ObjC") {
  // We use object_getClass in this test to not look through KVO's artificial
  // subclass.
  let obj = NSObject()
  let observation = obj.observe(\.description) { _, _ in }
  withExtendedLifetime(observation) {
    let theClass = object_getClass(obj)
    precondition(theClass !== NSObject.self, "no KVO subclass?")
    expectNil(class_getImageName(theClass),
              "should match what happens with the Swift objects (above)")
  }
}

testSuite.test("dynamic") {
  let newClass: AnyClass = objc_allocateClassPair(/*superclass*/nil,
                                                  "CompletelyDynamic",
                                                  /*extraBytes*/0)!
  objc_registerClassPair(newClass)

  // We don't actually care what the result is; we just need to not crash.
  _ = class_getImageName(newClass)
}

testSuite.test("nil") {
  // The ObjC runtime should handle this before it even gets to Swift's custom
  // implementation, but just in case.
  expectNil(class_getImageName(nil))
}

runAllTests()
