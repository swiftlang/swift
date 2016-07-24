// RUN: rm -rf %t && mkdir %t
// RUN: %target-build-swift -c -force-single-frontend-invocation -parse-as-library -emit-module -emit-module-path %t/PrintTestTypes.swiftmodule -o %t/PrintTestTypes.o %S/Inputs/PrintTestTypes.swift
// RUN: %target-build-swift %s -Xlinker %t/PrintTestTypes.o -I %t -L %t -o %t/main.out
// RUN: %target-run %t/main.out
// RUN: %target-run %t/main.out --locale ru_RU.UTF-8
// REQUIRES: executable_test

import StdlibUnittest
#if os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
  import Glibc
#else
  import Darwin
#endif
import PrintTestTypes


let PrintTests = TestSuite("PrintFloat")

PrintTests.setUp {
  if let localeArgIndex = CommandLine.arguments.index(of: "--locale") {
    let locale = CommandLine.arguments[localeArgIndex + 1]
    expectEqual("ru_RU.UTF-8", locale)
    setlocale(LC_ALL, locale)
  } else {
    setlocale(LC_ALL, "")
  }
}

PrintTests.test("CustomStringConvertible") {
  func hasDescription(_ any: Any) {
    expectTrue(any is CustomStringConvertible)
  }

  hasDescription(CFloat(1.0))
  hasDescription(CDouble(1.0))
}

PrintTests.test("Printable") {
  func asFloat32(_ f: Float32) -> Float32 { return f }
  func asFloat64(_ f: Float64) -> Float64 { return f }
#if arch(i386) || arch(x86_64)
  func asFloat80(_ f: Swift.Float80) -> Swift.Float80 { return f }
#endif

  expectPrinted("1.0", Float(1.0))
  expectPrinted("-1.0", Float(-1.0))
  expectPrinted("1.0", Double(1.0))
  expectPrinted("-1.0", Double(-1.0))

  expectPrinted("1.0", CFloat(1.0))
  expectPrinted("-1.0", CFloat(-1.0))
  expectPrinted("1.0", CDouble(1.0))
  expectPrinted("-1.0", CDouble(-1.0))

  expectPrinted("inf", Float.infinity)
  expectPrinted("-inf", -Float.infinity)
  expectPrinted("nan", Float.nan)
  expectPrinted("nan", -Float.nan)
  expectPrinted("nan", Float.signalingNaN)
  expectPrinted("nan", -Float.signalingNaN)
  expectPrinted("0.0", asFloat32(0.0))
  expectPrinted("1.0", asFloat32(1.0))
  expectPrinted("-1.0", asFloat32(-1.0))
  expectPrinted("100.125", asFloat32(100.125))
  expectPrinted("-100.125", asFloat32(-100.125))

  expectPrinted("inf", Double.infinity)
  expectPrinted("-inf", -Double.infinity)
  expectPrinted("nan", Double.nan)
  expectPrinted("nan", -Double.nan)
  expectPrinted("nan", Double.signalingNaN)
  expectPrinted("nan", -Double.signalingNaN)
  expectPrinted("0.0", asFloat64(0.0))
  expectPrinted("1.0", asFloat64(1.0))
  expectPrinted("-1.0", asFloat64(-1.0))
  expectPrinted("100.125", asFloat64(100.125))
  expectPrinted("-100.125", asFloat64(-100.125))

#if arch(i386) || arch(x86_64)
  expectPrinted("inf", Float80.infinity)
  expectPrinted("-inf", -Float80.infinity)
  expectPrinted("nan", Float80.nan)
  expectPrinted("nan", -Float80.nan)
  expectPrinted("nan", Float80.signalingNaN)
  expectPrinted("nan", -Float80.signalingNaN)
  expectPrinted("0.0", asFloat80(0.0))
  expectPrinted("1.0", asFloat80(1.0))
  expectPrinted("-1.0", asFloat80(-1.0))
  expectPrinted("100.125", asFloat80(100.125))
  expectPrinted("-100.125", asFloat80(-100.125))
#endif

  expectPrinted("1.00001", asFloat32(1.00001))
  expectPrinted("1.25e+17", asFloat32(125000000000000000.0))
  expectPrinted("1.25e+16", asFloat32(12500000000000000.0))
  expectPrinted("1.25e+15", asFloat32(1250000000000000.0))
  expectPrinted("1.25e+14", asFloat32(125000000000000.0))
  expectPrinted("1.25e+13", asFloat32(12500000000000.0))
  expectPrinted("1.25e+12", asFloat32(1250000000000.0))
  expectPrinted("1.25e+11", asFloat32(125000000000.0))
  expectPrinted("1.25e+10", asFloat32(12500000000.0))
  expectPrinted("1.25e+09", asFloat32(1250000000.0))
  expectPrinted("1.25e+08", asFloat32(125000000.0))
  expectPrinted("1.25e+07", asFloat32(12500000.0))
  expectPrinted("1.25e+06", asFloat32(1250000.0))
  expectPrinted("125000.0", asFloat32(125000.0))
  expectPrinted("12500.0",  asFloat32(12500.0))
  expectPrinted("1250.0",   asFloat32(1250.0))
  expectPrinted("125.0",    asFloat32(125.0))
  expectPrinted("12.5",     asFloat32(12.5))
  expectPrinted("1.25",     asFloat32(1.25))
  expectPrinted("0.125",    asFloat32(0.125))
  expectPrinted("0.0125",   asFloat32(0.0125))
  expectPrinted("0.00125",  asFloat32(0.00125))
  expectPrinted("0.000125", asFloat32(0.000125))
  expectPrinted("1.25e-05", asFloat32(0.0000125))
  expectPrinted("1.25e-06", asFloat32(0.00000125))
  expectPrinted("1.25e-07", asFloat32(0.000000125))
  expectPrinted("1.25e-08", asFloat32(0.0000000125))
  expectPrinted("1.25e-09", asFloat32(0.00000000125))
  expectPrinted("1.25e-10", asFloat32(0.000000000125))
  expectPrinted("1.25e-11", asFloat32(0.0000000000125))
  expectPrinted("1.25e-12", asFloat32(0.00000000000125))
  expectPrinted("1.25e-13", asFloat32(0.000000000000125))
  expectPrinted("1.25e-14", asFloat32(0.0000000000000125))
  expectPrinted("1.25e-15", asFloat32(0.00000000000000125))
  expectPrinted("1.25e-16", asFloat32(0.000000000000000125))
  expectPrinted("1.25e-17", asFloat32(0.0000000000000000125))

  expectPrinted("1.00000000000001", asFloat64(1.00000000000001))
  expectPrinted("1.25e+17", asFloat64(125000000000000000.0))
  expectPrinted("1.25e+16", asFloat64(12500000000000000.0))
  expectPrinted("1.25e+15", asFloat64(1250000000000000.0))
  expectPrinted("125000000000000.0", asFloat64(125000000000000.0))
  expectPrinted("12500000000000.0", asFloat64(12500000000000.0))
  expectPrinted("1250000000000.0", asFloat64(1250000000000.0))
  expectPrinted("125000000000.0", asFloat64(125000000000.0))
  expectPrinted("12500000000.0", asFloat64(12500000000.0))
  expectPrinted("1250000000.0", asFloat64(1250000000.0))
  expectPrinted("125000000.0", asFloat64(125000000.0))
  expectPrinted("12500000.0", asFloat64(12500000.0))
  expectPrinted("1250000.0", asFloat64(1250000.0))
  expectPrinted("125000.0", asFloat64(125000.0))
  expectPrinted("12500.0", asFloat64(12500.0))
  expectPrinted("1250.0", asFloat64(1250.0))
  expectPrinted("125.0", asFloat64(125.0))
  expectPrinted("12.5", asFloat64(12.5))
  expectPrinted("1.25", asFloat64(1.25))
  expectPrinted("0.125", asFloat64(0.125))
  expectPrinted("0.0125", asFloat64(0.0125))
  expectPrinted("0.00125", asFloat64(0.00125))
  expectPrinted("0.000125", asFloat64(0.000125))
  expectPrinted("1.25e-05", asFloat64(0.0000125))
  expectPrinted("1.25e-06", asFloat64(0.00000125))
  expectPrinted("1.25e-07", asFloat64(0.000000125))
  expectPrinted("1.25e-08", asFloat64(0.0000000125))
  expectPrinted("1.25e-09", asFloat64(0.00000000125))
  expectPrinted("1.25e-10", asFloat64(0.000000000125))
  expectPrinted("1.25e-11", asFloat64(0.0000000000125))
  expectPrinted("1.25e-12", asFloat64(0.00000000000125))
  expectPrinted("1.25e-13", asFloat64(0.000000000000125))
  expectPrinted("1.25e-14", asFloat64(0.0000000000000125))
  expectPrinted("1.25e-15", asFloat64(0.00000000000000125))
  expectPrinted("1.25e-16", asFloat64(0.000000000000000125))
  expectPrinted("1.25e-17", asFloat64(0.0000000000000000125))

#if arch(i386) || arch(x86_64)
  expectPrinted("1.00000000000000001", asFloat80(1.00000000000000001))
  expectPrinted("1.25e+19", asFloat80(12500000000000000000.0))
  expectPrinted("1.25e+18", asFloat80(1250000000000000000.0))
  expectPrinted("125000000000000000.0", asFloat80(125000000000000000.0))
  expectPrinted("12500000000000000.0", asFloat80(12500000000000000.0))
  expectPrinted("1250000000000000.0", asFloat80(1250000000000000.0))
  expectPrinted("125000000000000.0", asFloat80(125000000000000.0))
  expectPrinted("12500000000000.0", asFloat80(12500000000000.0))
  expectPrinted("1250000000000.0", asFloat80(1250000000000.0))
  expectPrinted("125000000000.0", asFloat80(125000000000.0))
  expectPrinted("12500000000.0", asFloat80(12500000000.0))
  expectPrinted("1250000000.0", asFloat80(1250000000.0))
  expectPrinted("125000000.0", asFloat80(125000000.0))
  expectPrinted("12500000.0", asFloat80(12500000.0))
  expectPrinted("1250000.0", asFloat80(1250000.0))
  expectPrinted("125000.0", asFloat80(125000.0))
  expectPrinted("12500.0", asFloat80(12500.0))
  expectPrinted("1250.0", asFloat80(1250.0))
  expectPrinted("125.0", asFloat80(125.0))
  expectPrinted("12.5", asFloat80(12.5))
  expectPrinted("1.25", asFloat80(1.25))
  expectPrinted("0.125", asFloat80(0.125))
  expectPrinted("0.0125", asFloat80(0.0125))
  expectPrinted("0.00125", asFloat80(0.00125))
  expectPrinted("0.000125", asFloat80(0.000125))
  expectPrinted("1.25e-05", asFloat80(0.0000125))
  expectPrinted("1.25e-06", asFloat80(0.00000125))
  expectPrinted("1.25e-07", asFloat80(0.000000125))
  expectPrinted("1.25e-08", asFloat80(0.0000000125))
  expectPrinted("1.25e-09", asFloat80(0.00000000125))
  expectPrinted("1.25e-10", asFloat80(0.000000000125))
  expectPrinted("1.25e-11", asFloat80(0.0000000000125))
  expectPrinted("1.25e-12", asFloat80(0.00000000000125))
  expectPrinted("1.25e-13", asFloat80(0.000000000000125))
  expectPrinted("1.25e-14", asFloat80(0.0000000000000125))
  expectPrinted("1.25e-15", asFloat80(0.00000000000000125))
  expectPrinted("1.25e-16", asFloat80(0.000000000000000125))
  expectPrinted("1.25e-17", asFloat80(0.0000000000000000125))
#endif

  expectDebugPrinted("1.10000002", asFloat32(1.1))
  expectDebugPrinted("1.24999998e+17", asFloat32(125000000000000000.0))
  expectDebugPrinted("1.25", asFloat32(1.25))
  expectDebugPrinted("1.24999997e-05", asFloat32(0.0000125))
  expectDebugPrinted("inf", Float.infinity)
  expectDebugPrinted("-inf", -Float.infinity)
  expectDebugPrinted("nan", Float.nan)
#if !arch(arm)
  expectDebugPrinted("-nan", -Float.nan)
#endif
  expectDebugPrinted("nan(0xffff)", Float(nan: 65535, signaling: false))
#if !arch(arm)
  expectDebugPrinted("-nan(0xffff)", -Float(nan: 65535, signaling: false)) // fail
#endif
  expectDebugPrinted("nan(0x1fffff)", Float(bitPattern: 0x7fff_ffff))
  expectDebugPrinted("nan(0x1fffff)", Float(bitPattern: 0x7fdf_ffff))
#if !arch(i386) && !arch(arm)
  expectDebugPrinted("snan", Float.signalingNaN)
  expectDebugPrinted("snan(0xffff)", Float(nan: 65535, signaling: true))
  expectDebugPrinted("-snan(0xffff)", -Float(nan: 65535, signaling: true))
  expectDebugPrinted("snan(0x1fffff)", Float(bitPattern: 0x7fbf_ffff))
#endif

  expectDebugPrinted("1.1000000000000001", asFloat64(1.1))
  expectDebugPrinted("1.25e+17", asFloat64(125000000000000000.0))
  expectDebugPrinted("1.25", asFloat64(1.25))
  expectDebugPrinted("1.2500000000000001e-05", asFloat64(0.0000125))
  expectDebugPrinted("inf", Double.infinity)
  expectDebugPrinted("-inf", -Double.infinity)
  expectDebugPrinted("nan", Double.nan)
  expectDebugPrinted("-nan", -Double.nan)
  expectDebugPrinted("nan(0xffff)", Double(nan: 65535, signaling: false))
  expectDebugPrinted("-nan(0xffff)", -Double(nan: 65535, signaling: false))
  expectDebugPrinted("nan(0x3ffffffffffff)", Float64(bitPattern: 0x7fff_ffff_ffff_ffff))
  expectDebugPrinted("nan(0x3ffffffffffff)", Float64(bitPattern: 0x7ffb_ffff_ffff_ffff))
#if !arch(i386)
  expectDebugPrinted("snan", Double.signalingNaN)
  expectDebugPrinted("snan(0xffff)", Double(nan: 65535, signaling: true))
  expectDebugPrinted("-snan(0xffff)", -Double(nan: 65535, signaling: true))
  expectDebugPrinted("snan(0x3ffffffffffff)", Float64(bitPattern: 0x7ff7_ffff_ffff_ffff))
#endif

#if arch(i386) || arch(x86_64)
  expectDebugPrinted("1.10000000000000000002", asFloat80(1.1))
  expectDebugPrinted("125000000000000000.0", asFloat80(125000000000000000.0))
  expectDebugPrinted("1.25", asFloat80(1.25))
  expectDebugPrinted("1.25000000000000000001e-05", asFloat80(0.0000125))
  expectDebugPrinted("inf", Float80.infinity)
  expectDebugPrinted("-inf", -Float80.infinity)
  expectDebugPrinted("nan", Float80.nan)
  expectDebugPrinted("-nan", -Float80.nan)
  expectDebugPrinted("nan(0xffff)", Float80(nan: 65535, signaling: false))
  expectDebugPrinted("-nan(0xffff)", -Float80(nan: 65535, signaling: false))
  expectDebugPrinted("nan(0x1fffffffffffffff)", Float80(sign: .plus, exponentBitPattern: 0x7fff, significandBitPattern: 0xffff_ffff_ffff_ffff))
  expectDebugPrinted("nan(0x1fffffffffffffff)", Float80(sign: .plus, exponentBitPattern: 0x7fff, significandBitPattern: 0xdfff_ffff_ffff_ffff))
  expectDebugPrinted("snan", Float80.signalingNaN)
  expectDebugPrinted("snan(0xffff)", Float80(nan: 65535, signaling: true))
  expectDebugPrinted("-snan(0xffff)", -Float80(nan: 65535, signaling: true))
  expectDebugPrinted("snan(0x1fffffffffffffff)", Float80(sign: .plus, exponentBitPattern: 0x7fff, significandBitPattern: 0xbfff_ffff_ffff_ffff))
#endif
}

runAllTests()
