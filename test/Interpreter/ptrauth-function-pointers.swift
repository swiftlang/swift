// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: CPU=arm64e

import StdlibUnittest

var PtrAuthFunctionPointersTestSuite = TestSuite("PtrAuthFunctionPointers")

func foo() {}
func bar() {}

struct FuncPtrs {
    var a: ()->()
    var b: ()->()
}

PtrAuthFunctionPointersTestSuite.test("PointerAreSigned") {
  var ptrs = FuncPtrs(a: foo, b: bar)
  withUnsafeBytes(of: &ptrs) { bytes in
      let p = bytes.load(fromByteOffset: 0, as: UInt.self)
      expectEqual(UInt.bitWidth, 64)
      let signature     = p & 0x00fffff0_00000000
      let actualPointer = p & 0x0000000f_ffffffff

      // The top byte of a signed function pointer actually being zero is only
      // guaranteed by ARMv8.3 if TBI is enabled, which it isn't for function
      // pointers in new iOS releases
      //expectEqual(topByte, 0)
      expectNotEqual(signature, 0)
      expectNotEqual(actualPointer, 0)
  }
}

runAllTests()
