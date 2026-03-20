// RUN: %target-run-simple-swift -Onone -g
// REQUIRES: executable_test

// rdar://77087867
// UNSUPPORTED: CPU=arm64_32 && OS=watchos

// Float16 not available in watchOS before watchOS 7.0
// UNSUPPORTED: DARWIN_SIMULATOR=watchos

// Float16 not currently available on macOS
// UNSUPPORTED: OS=macosx

// ABI-stable platforms don't support Float16 on x86_64 because Intel did not
// define a stable Float16 ABI for x86_64 until quite recently
// rdar://104232602
// UNSUPPORTED: CPU=x86_64 && (DARWIN_SIMULATOR=ios || DARWIN_SIMULATOR=watchos || DARWIN_SIMULATOR=tvos)

// This test contains a hacked-up copy of an old version of the Float16 formatter,
// and checks every Float16 to ensure that debugDescription returns the exact
// same string value as it always has.

// Several generations of Float16.debugDescription implementations
// have matched this exactly.

// In particular, note that for Float16, we've never required a truly minimum
// number of significant digits.  That's because "6.55e+04" (which has the
// actual fewest significant digits) is a lot harder to read and slower to
// produce than "65504.0".

import StdlibUnittest

let PrintTests = TestSuite("PrintFloat16_Comparison")

PrintTests.test("Exhaustive_Float16") {
  for i in 0...UInt32(0xffff) {
    let f = Float16(bitPattern: UInt16(truncatingIfNeeded: i))
    let expect = FormatFloat16(f)
    let actual = f.debugDescription
    let msg = "Correctness failure: \"\(actual)\" != \"\(expect)\" bitPattern: \(f.bitPattern)"
    expectEqual(expect, actual, msg)
  }
}

func FormatFloat16(_ f: Float16) -> String {
  if f.isInfinite {
    if f.sign == .minus { return "-inf" }
    else { return "inf" }
  }
  if f.isZero {
    if f.sign == .minus { return "-0.0" }
    else { return "0.0" }
  }
  if f.isNaN {
    return FormatNaN(f)
  }
  return _Float16ToASCII_Local(value: f)
}

func FormatNaN(_ f: Float16) -> String {
  var s = ""

  let quietBit =
    (f.significandBitPattern >> (Float16.significandBitCount - 1)) & 1
  let payloadMask = UInt16(1 &<< (Float16.significandBitCount - 2)) - 1
  let payload = f.significandBitPattern & payloadMask

  if f.sign == .minus { s = "-" }
  if quietBit == 0 { s += "s" }
  s += "nan"
  if payload != 0 {
    s += "(0x" + String(payload, radix: 16) + ")"
  }
  return s
}

@available(SwiftStdlib 5.3, *)
internal func _Float16ToASCII_Local(
  value f: Float16
) -> String {
  var buffer = ""

  // Step 1: Handle various input cases:
  // Note `FormatFloat16` has already handled 0/inf/NaN
  let binaryExponent: Int
  let significand: Float16.RawSignificand
  let exponentBias = (1 << (Float16.exponentBitCount - 1)) - 2 // 14
  if (f.exponentBitPattern == 0) {
    // Subnormal
    binaryExponent = 1 - exponentBias
    significand = f.significandBitPattern &<< 2
  } else { // normal
    binaryExponent = Int(f.exponentBitPattern) &- exponentBias
    let hiddenBit = Float16.RawSignificand(1) << Float16.significandBitCount
    significand = (f.significandBitPattern &+ hiddenBit) &<< 2
  }

  // Step 2: Determine the exact target interval
  let halfUlp: Float16.RawSignificand = 2
  let quarterUlp = halfUlp >> 1
  let upperMidpointExact =
    significand &+ halfUlp
  let lowerMidpointExact =
    significand &- ((f.significandBitPattern == 0) ? quarterUlp : halfUlp)

  // Step 3: If it's < 10^-5, format as exponential form
  if binaryExponent < -13 || (binaryExponent == -13 && significand < 0x1a38) {
    var decimalExponent = -5
    var u =
      (UInt32(upperMidpointExact) << (28 - 13 &+ binaryExponent)) &* 100000
    var l =
      (UInt32(lowerMidpointExact) << (28 - 13 &+ binaryExponent)) &* 100000
    var t =
      (UInt32(significand) << (28 - 13 &+ binaryExponent)) &* 100000
    let mask = (UInt32(1) << 28) - 1
    if t < ((1 << 28) / 10) {
      u &*= 100
      l &*= 100
      t &*= 100
      decimalExponent &-= 2
    }
    if t < (1 << 28) {
      u &*= 10
      l &*= 10
      t &*= 10
      decimalExponent &-= 1
    }
    let uDigit = u >> 28
    if uDigit == (l >> 28) {
      // More than one digit, so write first digit, ".", then the rest
      buffer += String(Unicode.Scalar(0x30 + uDigit)!)
      buffer += "."
      while true {
        u = (u & mask) &* 10
        l = (l & mask) &* 10
        t = (t & mask) &* 10
        let uDigit = u >> 28
        if uDigit != (l >> 28) {
          // Stop before emitting the last digit
          break
        }
        buffer += String(Unicode.Scalar(0x30 + uDigit)!)
      }
    }
    let digit = 0x30 &+ (t &+ (1 << 27)) >> 28
    buffer += String(Unicode.Scalar(digit)!)
    buffer += "e"
    buffer += "-"
    buffer += String(Unicode.Scalar(UInt8(truncatingIfNeeded: -decimalExponent / 10 &+ 0x30)))
    buffer += String(Unicode.Scalar(UInt8(truncatingIfNeeded: -decimalExponent % 10 &+ 0x30)))
  } else {

    // Step 4: Greater than 10^-5, so use decimal format "123.45"
    // First, split into integer and fractional parts:

    let intPart : Float16.RawSignificand
    let fractionPart : Float16.RawSignificand
    if binaryExponent < 13 {
      intPart = significand >> (13 &- binaryExponent)
      fractionPart = significand &- (intPart &<< (13 &- binaryExponent))
    } else {
      intPart = significand &<< (binaryExponent &- 13)
      fractionPart = significand &- (intPart >> (binaryExponent &- 13))
    }

    // Step 5: Emit the integer part
    buffer += String(intPart)
    buffer += "."

    if fractionPart == 0 {
      buffer += "0"
    } else {
      // Step 7: Emit the fractional part by repeatedly
      // multiplying by 10 to produce successive digits:
      var u = UInt32(upperMidpointExact) &<< (28 - 13 &+ binaryExponent)
      var l = UInt32(lowerMidpointExact) &<< (28 - 13 &+ binaryExponent)
      var t = UInt32(fractionPart) &<< (28 - 13 &+ binaryExponent)
      let mask = (UInt32(1) << 28) - 1
      var uDigit: UInt8 = 0
      var lDigit: UInt8 = 0
      while true {
        u = (u & mask) &* 10
        l = (l & mask) &* 10
        uDigit = UInt8(truncatingIfNeeded: u >> 28)
        lDigit = UInt8(truncatingIfNeeded: l >> 28)
        if uDigit != lDigit {
          t = (t & mask) &* 10
          break
        }
        // This overflows, but we don't care at this point.
        t &*= 10
        buffer += String(Unicode.Scalar(0x30 + uDigit))
      }
      t &+= 1 << 27
      if (t & mask) == 0 { // Exactly 1/2
        t = (t >> 28) & ~1 // Round last digit even
        // Correct if `t` is outside the rounding interval
        if t < lDigit || (t == lDigit && l > 0) {
	        t += 1
        }
      } else {
        t >>= 28
      }
      buffer += String(Unicode.Scalar(0x30 + t)!)
    }
  }
  if f.sign == .minus {
    buffer = "-" + buffer
  }
  return buffer
}

runAllTests()
