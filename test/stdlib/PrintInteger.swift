// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection

import StdlibUnittest


let PrintTests = TestSuite("PrintInteger")
PrintTests.test("CustomStringConvertible") {
  func hasDescription(_ any: Any) {
    expectTrue(any is CustomStringConvertible)
  }

  hasDescription(Int(42))
  hasDescription(UInt(42))

  hasDescription(Int8(-42))
  hasDescription(Int16(-42))
  hasDescription(Int32(-42))
  hasDescription(Int64(-42))
  hasDescription(UInt8(42))
  hasDescription(UInt16(42))
  hasDescription(UInt32(42))
  hasDescription(UInt64(42))

  hasDescription(CChar(42))
  hasDescription(CUnsignedChar(42))
  hasDescription(CUnsignedShort(42))
  hasDescription(CUnsignedInt(42))
  hasDescription(CUnsignedLong(42))
  hasDescription(CUnsignedLongLong(42))
  hasDescription(CSignedChar(42))
  hasDescription(CShort(42))
  hasDescription(CInt(42))
  hasDescription(CLong(42))
  hasDescription(CLongLong(42))
#if os(Windows)
  hasDescription(CWideChar(exactly: 42)!)
#else
  hasDescription(CWideChar(42)!)
#endif
  hasDescription(CChar8(42))
  hasDescription(CChar16(42))
  hasDescription(CChar32(42)!)
}

PrintTests.test("Printable") {
  expectPrinted("42", CChar(42))
  expectPrinted("42", CUnsignedChar(42))
  expectPrinted("42", CUnsignedShort(42))
  expectPrinted("42", CUnsignedInt(42))
  expectPrinted("42", CUnsignedLong(42))
  expectPrinted("42", CUnsignedLongLong(42))
  expectPrinted("42", CSignedChar(42))
  expectPrinted("42", CShort(42))
  expectPrinted("42", CInt(42))
  expectPrinted("42", CLong(42))
  expectPrinted("42", CLongLong(42))
#if os(Windows)
  expectPrinted("42", CWideChar(exactly: 42)!)
#else
  expectPrinted("*", CWideChar(42)!)
#endif
  expectPrinted("42", CChar8(42))
  expectPrinted("42", CChar16(42))
  expectPrinted("*", CChar32(42)!)

  if (UInt64(Int.max) > 0x1_0000_0000 as UInt64) {
    expectPrinted("-9223372036854775808", Int.min)
    expectPrinted("9223372036854775807", Int.max)
  } else {
    expectPrinted("-2147483648", Int.min)
    expectPrinted("2147483647", Int.max)
  }
  
  expectPrinted("0", Int(0))
  expectPrinted("42", Int(42))
  expectPrinted("-42", Int(-42))
  
  if (UInt64(UInt.max) > 0x1_0000_0000 as UInt64) {
    expectPrinted("18446744073709551615", UInt.max)
  } else {
    expectPrinted("4294967295", UInt.max)
  }
  
  expectPrinted("0", UInt.min)
  expectPrinted("0", UInt(0))
  expectPrinted("42", UInt(42))
  
  expectPrinted("-128", Int8.min)
  expectPrinted("127", Int8.max)
  expectPrinted("0", Int8(0))
  expectPrinted("42", Int8(42))
  expectPrinted("-42", Int8(-42))
  
  expectPrinted("0", UInt8.min)
  expectPrinted("255", UInt8.max)
  expectPrinted("0", UInt8(0))
  expectPrinted("42", UInt8(42))
  
  expectPrinted("-32768", Int16.min)
  expectPrinted("32767", Int16.max)
  expectPrinted("0", Int16(0))
  expectPrinted("42", Int16(42))
  expectPrinted("-42", Int16(-42))
  
  expectPrinted("0", UInt16.min)
  expectPrinted("65535", UInt16.max)
  expectPrinted("0", UInt16(0))
  expectPrinted("42", UInt16(42))
  
  expectPrinted("-2147483648", Int32.min)
  expectPrinted("2147483647", Int32.max)
  expectPrinted("0", Int32(0))
  expectPrinted("42", Int32(42))
  expectPrinted("-42", Int32(-42))
  
  expectPrinted("0", UInt32.min)
  expectPrinted("4294967295", UInt32.max)
  expectPrinted("0", UInt32(0))
  expectPrinted("42", UInt32(42))
  
  expectPrinted("-9223372036854775808", Int64.min)
  expectPrinted("9223372036854775807", Int64.max)
  expectPrinted("0", Int64(0))
  expectPrinted("42", Int64(42))
  expectPrinted("-42", Int64(-42))
  
  expectPrinted("0", UInt64.min)
  expectPrinted("18446744073709551615", UInt64.max)
  expectPrinted("0", UInt64(0))
  expectPrinted("42", UInt64(42))
  
  expectPrinted("-42", Int8(-42))
  expectPrinted("-42", Int16(-42))
  expectPrinted("-42", Int32(-42))
  expectPrinted("-42", Int64(-42))
  expectPrinted("42", UInt8(42))
  expectPrinted("42", UInt16(42))
  expectPrinted("42", UInt32(42))
  expectPrinted("42", UInt64(42))

  expectPrinted("42", CChar(42))
  expectPrinted("42", CUnsignedChar(42))
  expectPrinted("42", CUnsignedShort(42))
  expectPrinted("42", CUnsignedInt(42))
  expectPrinted("42", CUnsignedLong(42))
  expectPrinted("42", CUnsignedLongLong(42))
  expectPrinted("42", CSignedChar(42))
  expectPrinted("42", CShort(42))
  expectPrinted("42", CInt(42))
  expectPrinted("42", CLong(42))
  expectPrinted("42", CLongLong(42))

#if os(Windows)
  expectPrinted("42", CWideChar(exactly: 42)!)
#else
  expectPrinted("*", CWideChar(42)!)
#endif
  expectPrinted("42", CChar8(42))
  expectPrinted("42", CChar16(42))
  expectPrinted("*", CChar32(42)!)
}

runAllTests()
