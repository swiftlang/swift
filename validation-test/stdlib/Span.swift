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
    expectEqual(0x0100, rawSpan.unsafeLoad(as: UInt16.self))
  }
}

RawSpanTests.test(
  "RawSpan._Cursor.consumingLoad"
) {
  guard #available(SwiftStdlib 6.1, *) else { return }
  
  data.withUnsafeBytes { rawBuffer in
    let rawSpan = RawSpan(_unsafeBytes: rawBuffer)
    var cursor = rawSpan._startCursor()
    let value1 = rawSpan._consumingLoad(from: &cursor, as: UInt16.self)
    let value2: UInt16 = rawSpan._consumingLoad(from: &cursor)
    expectEqual(0x0100, value1)
    expectEqual(0x0302, value2)
    let remaining1 = cursor.distance(to: rawSpan._endCursor())
    let remaining2 = rawSpan._count(remainingAfter: cursor)
    expectEqual(252, remaining1)
    expectEqual(252, remaining2)
  }
}

RawSpanTests.test(
  "RawSpan._Cursor.uncheckedConsumingLoad"
) {
  guard #available(SwiftStdlib 6.1, *) else { return }
  
  data.withUnsafeBytes { rawBuffer in
    let rawSpan = RawSpan(_unsafeBytes: rawBuffer)
    var cursor = rawSpan._startCursor()
    let value1 = rawSpan._uncheckedConsumingLoad(from: &cursor, as: UInt16.self)
    let value2: UInt16 = rawSpan._uncheckedConsumingLoad(from: &cursor)
    expectEqual(0x0100, value1)
    expectEqual(0x0302, value2)
    let remaining1 = cursor.distance(to: rawSpan._endCursor())
    let remaining2 = rawSpan._count(remainingAfter: cursor)
    expectEqual(252, remaining1)
    expectEqual(252, remaining2)
  }
  
  data.withUnsafeBytes { rawBuffer in
    let rawSpan = RawSpan(_unsafeBytes: rawBuffer)
    var cursor = rawSpan.startCursor()
    var total: UInt64 = 0
    while !rawSpan.isAtEnd(cursor) {
      total += UInt64(rawSpan.uncheckedConsumingLoad(from: &cursor, as: UInt16.self))
    }
    expectEqual(0x403f80, total)
    let remaining1 = cursor.distance(to: rawSpan.endCursor())
    let remaining2 = rawSpan.count(remainingAfter: cursor)
    expectEqual(0, remaining1)
    expectEqual(0, remaining2)
    expectTrue(rawSpan.isAtEnd(cursor))
  }
}

runAllTests()

