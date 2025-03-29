// RUN: %empty-directory(%t)
//
// RUN: %target-clang -x c %S/Inputs/retain_release_wrappers.c -c -o %t/retain_release_wrappers.o
// RUN: %target-build-swift -enable-experimental-feature Extern %t/retain_release_wrappers.o %s -o %t/refcount_overflow
// RUN: %target-codesign %t/refcount_overflow
// RUN: %target-run %t/refcount_overflow

// REQUIRES: executable_test
// REQUIRES: swift_feature_Extern
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import StdlibUnittest

// Declarations of runtime ABI refcounting functions.

@_extern(c)
func wrapper_swift_retain_n(_ obj: UnsafeMutableRawPointer, _ n: UInt32) -> UnsafeMutableRawPointer
@_extern(c)
func wrapper_swift_release_n(_ obj: UnsafeMutableRawPointer, _ n: UInt32)
@_extern(c)
func wrapper_swift_nonatomic_retain_n(_ obj: UnsafeMutableRawPointer, _ n: UInt32) -> UnsafeMutableRawPointer
@_extern(c)
func wrapper_swift_nonatomic_release_n(_ obj: UnsafeMutableRawPointer, _ n: UInt32)

@_extern(c)
func wrapper_swift_unownedRetain_n(_ obj: UnsafeMutableRawPointer, _ n: UInt32) -> UnsafeMutableRawPointer
@_extern(c)
func wrapper_swift_unownedRelease_n(_ obj: UnsafeMutableRawPointer, _ n: UInt32)
@_extern(c)
func wrapper_swift_nonatomic_unownedRetain_n(_ obj: UnsafeMutableRawPointer, _ n: UInt32) -> UnsafeMutableRawPointer
@_extern(c)
func wrapper_swift_nonatomic_unownedRelease_n(_ obj: UnsafeMutableRawPointer, _ n: UInt32)


// Maximum legal retain count.
// 30 bits of extra retain count, plus 1 for the implicit retain.
let maxRC = (1 as UInt32) << 30

// Maximum retain count that fits inline on 32-bit.
let maxInlineRC32 = (1 as UInt32) << 22

// Maximum unowned count that fits inline on 32-bit.
let maxInlineURC32 = (1 as UInt32) << 7

var didDeinit = false

class C {
  deinit {
    didDeinit = true
  }
}

// Get a pointer to an object with a single strong retain count. This prevents
// ARC from doing different things to us depending on optimization level.
func getTestObject() -> UnsafeMutableRawPointer {
  let obj = C()
  let ptr = unsafeBitCast(obj, to: UnsafeMutableRawPointer.self)
  _ = wrapper_swift_retain_n(ptr, 1)
  return ptr
}

// Balance the retain from getTestObject.
func releaseTestObject(_ obj: UnsafeMutableRawPointer) {
  wrapper_swift_release_n(obj, 1)
}

let RefcountOverflowTests = TestSuite("RefcountOverflow")

RefcountOverflowTests.test("retain") {
  do {
    let obj = getTestObject()
    _ = wrapper_swift_retain_n(obj, maxRC - 1)
    wrapper_swift_release_n(obj, maxRC - 1)
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("retain2") {
  do {
    let obj = getTestObject()
    _ = wrapper_swift_retain_n(obj, maxRC - 1)
    wrapper_swift_release_n(obj, maxRC - 1)
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("nonatomic_retain") {
  do {
    let obj = getTestObject()
    _ = wrapper_swift_nonatomic_retain_n(obj, maxRC - 1)
    wrapper_swift_nonatomic_release_n(obj, maxRC - 1)
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("retain one by one") {
  do {
    // Retain to near maxInlineRC32, then +1 over the limit.
    let obj = getTestObject()
    _ = wrapper_swift_retain_n(obj, maxInlineRC32 - 100)

    for _ in 0..<200 {
      _ = wrapper_swift_retain_n(obj, 1)
    }
    for _ in 0..<200 {
      wrapper_swift_release_n(obj, 1)
    }

    wrapper_swift_release_n(obj, maxInlineRC32 - 100)

    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("nonatomic_retain one by one") {
  do {
    // Retain to near maxInlineRC32, then +1 over the limit.
    let obj = getTestObject()
    _ = wrapper_swift_nonatomic_retain_n(obj, maxInlineRC32 - 100)

    for _ in 0..<200 {
      _ = wrapper_swift_nonatomic_retain_n(obj, 1)
    }
    for _ in 0..<200 {
      wrapper_swift_nonatomic_release_n(obj, 1)
    }

    wrapper_swift_nonatomic_release_n(obj, maxInlineRC32 - 100)

    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("unownedRetain") {
  do {
    let obj = getTestObject()
    _ = wrapper_swift_unownedRetain_n(obj, maxRC - 1)
    wrapper_swift_unownedRelease_n(obj, maxRC - 1)
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("nonatomic_unownedRetain") {
  do {
    let obj = getTestObject()
    _ = wrapper_swift_nonatomic_unownedRetain_n(obj, maxRC - 1)
    wrapper_swift_nonatomic_unownedRelease_n(obj, maxRC - 1)
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("unownedRetain one by one") {
  do {
    let obj = getTestObject()
    let n = maxInlineURC32 * 2
    for _ in 0..<n {
      _ = wrapper_swift_unownedRetain_n(obj, 1)
    }
    for _ in 0..<n {
      wrapper_swift_unownedRelease_n(obj, 1)
    }
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("nonatomic_unownedRetain one by one") {
  do {
    let obj = getTestObject()
    let n = maxInlineURC32 * 2
    for _ in 0..<n {
      _ = wrapper_swift_nonatomic_unownedRetain_n(obj, 1)
    }
    for _ in 0..<n {
      wrapper_swift_nonatomic_unownedRelease_n(obj, 1)
    }
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("unownedRetain moderate increment") {
  do {
    let obj = getTestObject()
    let n = maxInlineURC32 * 3 / 4
    _ = wrapper_swift_unownedRetain_n(obj, n)
    _ = wrapper_swift_unownedRetain_n(obj, n)
    wrapper_swift_unownedRelease_n(obj, n)
    wrapper_swift_unownedRelease_n(obj, n)
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

RefcountOverflowTests.test("nonatomic_unownedRetain moderate increment") {
  do {
    let obj = getTestObject()
    let n = maxInlineURC32 * 3 / 4
    _ = wrapper_swift_nonatomic_unownedRetain_n(obj, n)
    _ = wrapper_swift_nonatomic_unownedRetain_n(obj, n)
    wrapper_swift_nonatomic_unownedRelease_n(obj, n)
    wrapper_swift_nonatomic_unownedRelease_n(obj, n)
    releaseTestObject(obj)
  }
  expectTrue(didDeinit)
  didDeinit = false
}

runAllTests()
