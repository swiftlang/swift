// RUN: %empty-directory(%t)
//
// RUN: %target-clang -x c %S/Inputs/retain_release_wrappers.c -c -o %t/retain_release_wrappers.o
// RUN: %target-build-swift -enable-experimental-feature Extern %t/retain_release_wrappers.o %s -o %t/refcount_bridgeobject
// RUN: %target-codesign %t/refcount_bridgeobject
// RUN: %target-run %t/refcount_bridgeobject

// REQUIRES: executable_test
// REQUIRES: swift_feature_Extern
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

// Declarations of runtime ABI refcounting functions.

@_extern(c)
func wrapper_swift_bridgeObjectRetain(_ obj: UnsafeMutableRawPointer?) -> UnsafeMutableRawPointer?
@_extern(c)
func wrapper_swift_bridgeObjectRelease(_ obj: UnsafeMutableRawPointer?)

let RefcountBridgeObjectTests = TestSuite("RefcountBridgeObject")

var didDeinit = false

class C {
  deinit {
    didDeinit = true
  }
}

RefcountBridgeObjectTests.test("retain/release") {
  do {
    let obj = C()
    let asInt = unsafeBitCast(obj, to: UInt.self)
    // 2 is a spare bit available to BridgeObject on all current targets.
    let asBridgeObject = UnsafeMutableRawPointer(bitPattern: asInt | 2)

    let result = wrapper_swift_bridgeObjectRetain(asBridgeObject)
    expectEqual(asBridgeObject, result)

    wrapper_swift_bridgeObjectRelease(asBridgeObject)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

runAllTests()
