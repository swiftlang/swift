// RUN: %target-run-simple-swift
// REQUIRES: executable_test

//
// Tests for the non-Foundation CString-oriented API of String
//

import StdlibUnittest

var CStringTests = TestSuite("CStringTests")

func getNullUTF8() -> UnsafeMutablePointer<UInt8>? {
  return nil
}

func getASCIIUTF8() -> (UnsafeMutablePointer<UInt8>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  up[0] = 0x61
  up[1] = 0x62
  up[2] = 0
  return (up, { up.deallocate() })
}

func getNonASCIIUTF8() -> (UnsafeMutablePointer<UInt8>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  up[0] = 0xd0
  up[1] = 0xb0
  up[2] = 0xd0
  up[3] = 0xb1
  up[4] = 0
  return (UnsafeMutablePointer(up), { up.deallocate() })
}

func getIllFormedUTF8String1(
) -> (UnsafeMutablePointer<UInt8>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x80
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.deallocate() })
}

func getIllFormedUTF8String2(
) -> (UnsafeMutablePointer<UInt8>, dealloc: () -> ()) {
  let up = UnsafeMutablePointer<UInt8>.allocate(capacity: 100)
  up[0] = 0x41
  up[0] = 0x41
  up[1] = 0xed
  up[2] = 0xa0
  up[3] = 0x81
  up[4] = 0x41
  up[5] = 0
  return (UnsafeMutablePointer(up), { up.deallocate() })
}

func asCCharArray(_ a: [UInt8]) -> [CChar] {
  return a.map { CChar(bitPattern: $0) }
}

func getUTF8Length(_ cString: UnsafePointer<UInt8>) -> Int {
  var length = 0
  while cString[length] != 0 {
    length += 1
  }
  return length
}

func bindAsCChar(_ utf8: UnsafePointer<UInt8>) -> UnsafePointer<CChar> {
  return UnsafeRawPointer(utf8).bindMemory(to: CChar.self,
    capacity: getUTF8Length(utf8))
}

func expectEqualCString(_ lhs: UnsafePointer<UInt8>,
  _ rhs: UnsafePointer<UInt8>) {

  var index = 0
  while lhs[index] != 0 {
    expectEqual(lhs[index], rhs[index])
    index += 1
  }
  expectEqual(0, rhs[index])
}

func expectEqualCString(_ lhs: UnsafePointer<UInt8>,
  _ rhs: ContiguousArray<UInt8>) {
  rhs.withUnsafeBufferPointer {
    expectEqualCString(lhs, $0.baseAddress!)
  }
}

func expectEqualCString(_ lhs: UnsafePointer<UInt8>,
  _ rhs: ContiguousArray<CChar>) {
  rhs.withUnsafeBufferPointer {
    $0.baseAddress!.withMemoryRebound(
      to: UInt8.self, capacity: rhs.count) {
      expectEqualCString(lhs, $0)
    }
  }
}

CStringTests.test("String.init(validatingUTF8:)") {
  do {
    let (s, dealloc) = getASCIIUTF8()
    expectEqual("ab", String(validatingUTF8: bindAsCChar(s)))
    dealloc()
  }
  do {
    let (s, dealloc) = getNonASCIIUTF8()
    expectEqual("аб", String(validatingUTF8: bindAsCChar(s)))
    dealloc()
  }
  do {
    let (s, dealloc) = getIllFormedUTF8String1()
    expectNil(String(validatingUTF8: bindAsCChar(s)))
    dealloc()
  }
}

CStringTests.test("String(cString:)") {
  do {
    let (s, dealloc) = getASCIIUTF8()
    let result = String(cString: s)
    expectEqual("ab", result)
    let su = bindAsCChar(s)
    expectEqual("ab", String(cString: su))
    dealloc()
  }
  do {
    let (s, dealloc) = getNonASCIIUTF8()
    let result = String(cString: s)
    expectEqual("аб", result)
    let su = bindAsCChar(s)
    expectEqual("аб", String(cString: su))
    dealloc()
  }
  do {
    let (s, dealloc) = getIllFormedUTF8String1()
    let result = String(cString: s)
    expectEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", result)
    let su = bindAsCChar(s)
    expectEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", String(cString: su))
    dealloc()
  }
}

CStringTests.test("String.decodeCString") {
  do {
    let s = getNullUTF8()
    let result = String.decodeCString(s, as: UTF8.self)
    expectNil(result)
  }
  do { // repairing
    let (s, dealloc) = getIllFormedUTF8String1()
    if let (result, repairsMade) = String.decodeCString(
      s, as: UTF8.self, repairingInvalidCodeUnits: true) {
      expectEqual("\u{41}\u{fffd}\u{fffd}\u{fffd}\u{41}", result)
      expectTrue(repairsMade)
    } else {
      expectUnreachable("Expected .some()")
    }
    dealloc()
  }
  do { // non repairing
    let (s, dealloc) = getIllFormedUTF8String1()
    let result = String.decodeCString(
      s, as: UTF8.self, repairingInvalidCodeUnits: false)
    expectNil(result)
    dealloc()
  }
}

CStringTests.test("String.utf8CString") {
  do {
    let (cstr, dealloc) = getASCIIUTF8()
    defer { dealloc() }
    let str = String(cString: cstr)
    expectEqualCString(cstr, str.utf8CString)
  }
  do {
    let (cstr, dealloc) = getNonASCIIUTF8()
    defer { dealloc() }
    let str = String(cString: cstr)
    expectEqualCString(cstr, str.utf8CString)
  }
}

CStringTests.test("String.withCString") {
  do {
    let (cstr, dealloc) = getASCIIUTF8()
    defer { dealloc() }
    let str = String(cString: cstr)
    str.withCString {
      expectEqual(str, String(cString: $0))
    }
  }
  do {
    let (cstr, dealloc) = getASCIIUTF8()
    defer { dealloc() }
    let str = String(cString: cstr)
    str.withCString {
      expectEqual(str, String(cString: $0))
    }
  }
}

CStringTests.test("Substring.withCString") {
  do {
    let (cstr, dealloc) = getASCIIUTF8()
    defer { dealloc() }
    let str = String(cString: cstr).dropFirst()
    str.withCString {
      expectEqual(str, String(cString: $0))
    }
  }
  do {
    let (cstr, dealloc) = getASCIIUTF8()
    defer { dealloc() }
    let str = String(cString: cstr).dropFirst()
    str.withCString {
      expectEqual(str, String(cString: $0))
    }
  }
}

runAllTests()

