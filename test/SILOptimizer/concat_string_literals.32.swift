// RUN: %target-swift-frontend -O -emit-ir  %s | %FileCheck %s
// RUN: %target-swift-frontend -Osize -emit-ir  %s | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// We have a separate test for 64-bit architectures.
// REQUIRES: PTRSIZE=32

// NOTE: 25185.byteSwapped = 0x62 'a', 0x61 'b'
// CHECK-LABEL: test_ascii_scalar_scalar2
// CHECK: ret { i32, i32, i32 } { i32 25185, i32 0, i32 {{[0-9]+}} }
public func test_ascii_scalar_scalar2() -> String {
  return "a" + "b"
}


// NOTE: 11125601.byteSwapped = 0x61 'a', 0xC3 0xA9 'é'
// CHECK-LABEL: test_scalar_otherscalar
// CHECK: ret { i32, i32, i32 } { i32 11125601, i32 0, i32 {{[0-9]+}} }
public func test_scalar_otherscalar() -> String {
  return "a" + "é"
}

// NOTE: -8097488946593795999.byteSwapped = 0x61 'a', 0xF0 0x9F 0x95 0xb4 '🕴', ...
// NOTE: -8097488946593795999 = 0x8f9ff0b4959ff061
// NOTE: -1784680351 = 0x959ff061, -1885343564 = 0x8f9ff0b4
// CHECK-LABEL: test_scalar_char
// CHECK: ret { i32, i32, i32 } { i32 -1784680351, i32 -1885343564, i32 {{[0-9]+}} }
public func test_scalar_char() -> String {
  return "a" + "🕴🏿"
}

// NOTE: 112585666577249.byteSwapped = 0x61 'a', 0xc3 0xa9 'é', 0x64 'd', 0x65 'e', 0x66 'f'
// NOTE: 112585666577249 = 1688847201 + (26213 << 32)
// CHECK-LABEL: test_strng_strng2
// CHECK: ret { i32, i32, i32 } { i32 1688847201, i32 26213, i32 {{[0-9]+}} }
public func test_strng_strng2() -> String {
  return "aé" + "def"
}

// NOTE: 43 = code-unit length
// NOTE: 20 = native bias
// CHECK-LABEL: test_scalar_strng
// CHECK: ret { i32, i32, i32 } { i32 43, i32 sub (i32 {{.*}}, i32 20)
public func test_scalar_strng() -> String {
  return "a" + "👨🏿‍💼+🧙🏿‍♂️=🕴🏿"
}

// NOTE: 7450828190687388257.byteSwapped = 0x61 'a', 0x62 'b', 0x63 'c', 0x64 'd', 0xC3 0xA8 'è', 0x66 'f', 0x67 'g', ...
// NOTE: 1684234849 = 0x64636261, 1734781123 = 0x6766a8c3
// CHECK-LABEL: test_strng_concat_smol
// CHECK: ret { i32, i32, i32 } { i32 1684234849, i32 1734781123,
public func test_strng_concat_smol() -> String {
  return "a" + "bc" + "dèf" + "gĥ"
}

// NOTE: 11 = code-unit length
// NOTE: 20 = native bias
// CHECK-LABEL: test_strng_concat_not_quite_smol
// CHECK: ret { i32, i32, i32 } { i32 11, i32 sub (i32 {{.*}}, i32 20)
public func test_strng_concat_not_quite_smol() -> String {
  return "a" + "bc" + "dèf" + "ghī"
}

// NOTE: 23 = code-unit length
// NOTE: 20 = native bias
// CHECK-LABEL: test_strng_concat_large
// CHECK: ret { i32, i32, i32 } { i32 23, i32 sub (i32 {{.*}}, i32 20)
public func test_strng_concat_large() -> String {
  return "a" + "bc" + "dèf" + "ghī" + "jklmn" + "o" + "𝛒qr"
}
