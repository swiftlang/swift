// RUN: %target-run-simple-swift \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature Span
// REQUIRES: executable_test

import StdlibUnittest

var RawSpanTests = TestSuite(
  "RawSpanTests"
)

let data: [UInt8] = Array(0...255)

RawSpanTests.test(
  "RawSpan(_unsafeBytes:)"
) {
  guard #available(SwiftStdlib 6.1, *) else { return }
  
  data.withUnsafeBytes { rawBuffer in
    let rawSpan = RawSpan(_unsafeBytes: rawBuffer)
    expectEqual(0x0100, rawSpan.unsafeLoad(as: UInt16.self).littleEndian)
  }
}

RawSpanTests.test(
  "RawSpan._Cursor._unsafeLoad"
) {
  guard #available(SwiftStdlib 6.1, *) else { return }
  
  data.withUnsafeBytes { rawBuffer in
    let rawSpan = RawSpan(_unsafeBytes: rawBuffer)
    var cursor = rawSpan._startCursor()
    let value1 = rawSpan._unsafeLoad(advancing: &cursor, as: UInt16.self)
      .littleEndian
    let value2 = rawSpan._unsafeLoad(advancing: &cursor, as: UInt16.self)
      .littleEndian
    expectEqual(0x0100, value1)
    expectEqual(0x0302, value2)
    let remaining1 = cursor.distance(to: rawSpan._endCursor())
    let remaining2 = rawSpan._count(remainingAfter: cursor)
    expectEqual(252, remaining1)
    expectEqual(252, remaining2)
  }
}

RawSpanTests.test(
  "RawSpan._Cursor._unsafeUncheckedLoad"
) {
  guard #available(SwiftStdlib 6.1, *) else { return }
  
  data.withUnsafeBytes { rawBuffer in
    let rawSpan = RawSpan(_unsafeBytes: rawBuffer)
    var cursor = rawSpan._startCursor()
    let value1 = rawSpan._unsafeUncheckedLoad(advancing: &cursor, as: UInt16.self)
      .bigEndian
    let value2 = rawSpan._unsafeUncheckedLoad(advancing: &cursor, as: UInt16.self)
      .bigEndian
    expectEqual(0x0001, value1)
    expectEqual(0x0203, value2)
    let remaining1 = cursor.distance(to: rawSpan._endCursor())
    let remaining2 = rawSpan._count(remainingAfter: cursor)
    expectEqual(252, remaining1)
    expectEqual(252, remaining2)
  }
  
  data.withUnsafeBytes { rawBuffer in
    let rawSpan = RawSpan(_unsafeBytes: rawBuffer)
    var cursor = rawSpan._startCursor()
    var total: UInt64 = 0
    while !rawSpan._isAtEnd(cursor) {
      total += UInt64(rawSpan._unsafeUncheckedLoad(advancing: &cursor, as: UInt16.self))
    }
    expectEqual(0x403f80, total)
    let remaining1 = cursor.distance(to: rawSpan._endCursor())
    let remaining2 = rawSpan._count(remainingAfter: cursor)
    expectEqual(0, remaining1)
    expectEqual(0, remaining2)
    expectTrue(rawSpan._isAtEnd(cursor))
  }
}

RawSpanTests.test(
  "RawSpan._Cursor.offsets"
) {
  guard #available(SwiftStdlib 6.1, *) else { return }
  
  data.withUnsafeBytes { rawBuffer in
    let rawSpan = RawSpan(_unsafeBytes: rawBuffer)
    var cursor = rawSpan._startCursor()
    rawSpan._offsetCursor(&cursor, byByteCount: 2)
    let value1 = rawSpan._unsafeLoad(advancing: &cursor, as: UInt16.self)
      .bigEndian
    expectEqual(0x0203, value1)

    rawSpan._offsetCursor(&cursor, byByteCount: 10)
    expectEqual(242, rawSpan._count(remainingAfter: cursor))
    
    rawSpan._unsafeUncheckedOffsetCursor(&cursor, byByteCount: 10)
    expectEqual(232, rawSpan._count(remainingAfter: cursor))
    
    var cursor2 = rawSpan._endCursor()
    rawSpan._offsetCursor(&cursor2, byByteCount: -2)
    let value2 = rawSpan._unsafeLoad(advancing: &cursor2, as: UInt16.self)
      .bigEndian
    expectEqual(0xFEFF, value2)
    expectEqual(0, rawSpan._count(remainingAfter: cursor2))
    expectTrue(rawSpan._isAtEnd(cursor2))
    
    expectCrashLater()
    rawSpan._offsetCursor(&cursor2, byByteCount: 1)
  }
}

runAllTests()

