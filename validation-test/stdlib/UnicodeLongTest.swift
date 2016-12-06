// RUN: %target-run-simple-swift
// REQUIRES: long_test
// REQUIRES: executable_test

import SwiftPrivate
import StdlibUnittest

var UTF8Decoder = TestSuite("UTF8Decoder")

UTF8Decoder.test("Internal/_decodeOne") {

  // Ensure we accept all valid scalars
  func ensureValid(_ scalar: UnicodeScalar) {
    var data: UInt32 = 0
    var i: UInt32 = 0
    Swift.UTF8.encode(scalar) { cp in
      data |= UInt32(cp) << (i*8)
      i += 1
    }
    let (codePoint, _) = UTF8._decodeOne(data)
    expectOptionalEqual(scalar.value, codePoint, "data=\(asHex(data))")
  }

  for i in 0..<0xd800 { ensureValid(UnicodeScalar(i)!) }
  for i in 0xe000...0x10ffff { ensureValid(UnicodeScalar(i)!) }

  // Check number of valid/invalid sequences of different lengths
  var validLengthCounts = [ 0, 0, 0, 0, 0 ]
  var maximalSubpartCounts = [ 0, 0, 0, 0, 0 ]
  func countValidSequences(
    head: CountableClosedRange<UInt32>, tail: CountableClosedRange<UInt32>
  ) {
    for cu0 in head {
      for rest in tail {
        let data = rest << 8 | cu0
        let (codePoint, length) = UTF8._decodeOne(data)
        if codePoint != nil {
          validLengthCounts[Int(length)] += 1
        } else {
          maximalSubpartCounts[Int(length)] += 1
        }
      }
    }
  }

  countValidSequences(head: 0x00...0x7f, tail: 0...0)
  countValidSequences(head: 0xc0...0xdf, tail: 0...0xff)
  countValidSequences(head: 0xe0...0xef, tail: 0...0xffff)
  countValidSequences(head: 0xf0...0xff, tail: 0...0xffffff)

  // Valid sequences
  expectEqualSequence(validLengthCounts,
    [ 0, 0x80, 0x780, 0xf000, 0x100000 ],
    "validLengthCounts=\(validLengthCounts.map { asHex($0) })")
  // Maximal subparts of ill-formed sequences
  expectEqualSequence(maximalSubpartCounts,
    [ 0, 0xf0c5880, 0xc2d000, 0x300000, 0 ],
    "maximalSubpartCounts=\(maximalSubpartCounts.map { asHex($0) })")
}

runAllTests()

