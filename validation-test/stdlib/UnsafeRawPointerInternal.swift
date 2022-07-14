// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -o %t.out
// RUN: %target-codesign %t.out
// RUN: %target-run %t.out
// REQUIRES: executable_test
// UNSUPPORTED: freestanding

import Swift
import StdlibUnittest

var UnsafeRawPointerTestSuite =
  TestSuite("UnsafeRawPointerTestSuite")

UnsafeRawPointerTestSuite.test("load.unaligned.SIMD")
.skip(.custom({
  if #available(SwiftStdlib 5.7, *) { return false }
  return true
}, reason: "Requires Swift 5.7's stdlib"))
.code {
  guard #available(SwiftStdlib 5.7, *) else { return }
  var bytes: [UInt8] = Array(0..<64)
  var offset = 3
  let vector16 = bytes.withUnsafeBytes { buffer -> SIMD16<UInt8> in
    let aligned = buffer.baseAddress!.alignedUp(for: SIMD16<UInt8>.self)
    offset += buffer.baseAddress!.distance(to: aligned)
    return buffer.loadUnaligned(fromByteOffset: offset, as: SIMD16<UInt8>.self)
  }
  expectEqual(Int(vector16[0]), offset)
  var lastIndex = vector16.indices.last!
  expectEqual(Int(vector16[lastIndex]), offset+lastIndex)

  offset = 11
  let vector32 = bytes.withUnsafeMutableBytes { buffer -> SIMD32<UInt8> in
    let aligned = buffer.baseAddress!.alignedUp(for: SIMD32<UInt8>.self)
    offset += buffer.baseAddress!.distance(to: aligned)
    return buffer.loadUnaligned(fromByteOffset: offset, as: SIMD32<UInt8>.self)
  }
  expectEqual(Int(vector32[0]), offset)
  lastIndex = vector32.indices.last!
  expectEqual(Int(vector32[lastIndex]), offset+lastIndex)
}

runAllTests()
