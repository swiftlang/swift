// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify
//
// REQUIRES: PTRSIZE=64
//
// This file tests diagnostics for arithmetic and bitwise operations on `Int`
// and `UInt` types for 64 bit architectures.
//
// FIXME: This test should be merged back into
// diagnostic_constant_propagation.swift when we have fixed:
// <rdar://problem/19434979> -verify does not respect #if
//
// FIXME: <rdar://problem/29937936> False negatives when using integer initializers
//
// FIXME: <rdar://problem/39193272> A false negative that happens only in REPL

func testArithmeticOverflow_Int_64bit() {
  do {
    // Literals.
    var _: Int = 0x7fff_ffff_ffff_ffff // OK
    var _: Int = 0x8000_0000_0000_0000 // expected-error {{integer literal '9223372036854775808' overflows when stored into 'Int'}}

    var _: Int = -0x8000_0000_0000_0000 // OK
    var _: Int = -0x8000_0000_0000_0001 // expected-error {{integer literal '-9223372036854775809' overflows when stored into 'Int'}}
  }

  do {
    // Negation.
    var _: Int = -(-0x7fff_ffff_ffff_ffff) // OK
    var _: Int = -(-0x8000_0000_0000_0000) // expected-error {{arithmetic operation '0 - -9223372036854775808' (on signed 64-bit integer type) results in an overflow}}
    // FIXME: Missing diagnostic in REPL:
    // <rdar://problem/39193272> Overflow in arithmetic negation is not detected
    // at compile time when running in REPL
  }

  do {
    // Addition.
    var _: Int = 0x7fff_ffff_ffff_fffe + 1 // OK
    var _: Int = 0x7fff_ffff_ffff_fffe + 2 // expected-error {{arithmetic operation '9223372036854775806 + 2' (on type 'Int') results in an overflow}}

    var _: Int = -0x7fff_ffff_ffff_ffff + (-1) // OK
    var _: Int = -0x7fff_ffff_ffff_ffff + (-2) // expected-error {{arithmetic operation '-9223372036854775807 + -2' (on type 'Int') results in an overflow}}
  }

  do {
    // Subtraction.
    var _: Int = 0x7fff_ffff_ffff_fffe - (-1) // OK
    var _: Int = 0x7fff_ffff_ffff_fffe - (-2) // expected-error {{arithmetic operation '9223372036854775806 - -2' (on type 'Int') results in an overflow}}

    var _: Int = -0x7fff_ffff_ffff_ffff - 1 // OK
    var _: Int = -0x7fff_ffff_ffff_ffff - 2 // expected-error {{arithmetic operation '-9223372036854775807 - 2' (on type 'Int') results in an overflow}}
  }

  do {
    // Multiplication.
    var _: Int = 0x7fff_ffff_ffff_fffe * 1 // OK
    var _: Int = 0x7fff_ffff_ffff_fffe * 2 // expected-error {{arithmetic operation '9223372036854775806 * 2' (on type 'Int') results in an overflow}}

    var _: Int = -0x7fff_ffff_ffff_ffff * 1 // OK
    var _: Int = -0x7fff_ffff_ffff_ffff * 2 // expected-error {{arithmetic operation '-9223372036854775807 * 2' (on type 'Int') results in an overflow}}
  }

  do {
    // Division.
    var _: Int = 0x7fff_ffff_ffff_fffe / 2 // OK
    var _: Int = 0x7fff_ffff_ffff_fffe / 0 // expected-error {{division by zero}}

    var _: Int = -0x7fff_ffff_ffff_ffff / 2 // OK
    var _: Int = -0x7fff_ffff_ffff_ffff / 0 // expected-error {{division by zero}}

    var _: Int = -0x8000_0000_0000_0000 / -1 // expected-error {{division '-9223372036854775808 / -1' results in an overflow}}
  }

  do {
    // Remainder.
    var _: Int = 0x7fff_ffff_ffff_fffe % 2 // OK
    var _: Int = 0x7fff_ffff_ffff_fffe % 0 // expected-error {{division by zero}}

    var _: Int = -0x7fff_ffff_ffff_ffff % 2 // OK
    var _: Int = -0x7fff_ffff_ffff_ffff % 0 // expected-error {{division by zero}}

    var _: Int = -0x8000_0000_0000_0000 % -1 // expected-error {{division '-9223372036854775808 % -1' results in an overflow}}
  }

  do {
    // Right shift.
    // Due to "smart shift" introduction, there can be no overflows errors
    // during shift operations
    var _: Int = 0 >> 0
    var _: Int = 0 >> 1
    var _: Int = 0 >> (-1)

    var _: Int = 123 >> 0
    var _: Int = 123 >> 1
    var _: Int = 123 >> (-1)

    var _: Int = (-1) >> 0
    var _: Int = (-1) >> 1

    var _: Int = 0x7fff_ffff_ffff_ffff >> 63
    var _ :Int = 0x7fff_ffff_ffff_ffff >> 64
    var _ :Int = 0x7fff_ffff_ffff_ffff >> 65
  }

  do {
    // Left shift.
    // Due to "smart shift" introduction, there can be no overflows errors
    // during shift operations
    var _: Int = 0 << 0
    var _: Int = 0 << 1
    var _: Int = 0 << (-1)

    var _: Int = 123 << 0
    var _: Int = 123 << 1
    var _: Int = 123 << (-1)

    var _: Int = (-1) << 0
    var _: Int = (-1) << 1

    var _: Int = 0x7fff_ffff_ffff_ffff << 63
    var _ :Int = 0x7fff_ffff_ffff_ffff << 64
    var _ :Int = 0x7fff_ffff_ffff_ffff << 65
  }

  do {
    var _ : Int = ~0
    var _ : Int = (0x7fff_ffff_ffff_fff) | (0x4000_0000_0000_0000 << 1)
    var _ : Int = (0x7fff_ffff_ffff_ffff) | 0x8000_0000_0000_0000 // expected-error {{integer literal '9223372036854775808' overflows when stored into 'Int'}}
  }
}

func testArithmeticOverflow_UInt_64bit() {
  do {
    // Literals.
    var _: UInt = 0x7fff_ffff_ffff_ffff // OK
    var _: UInt = 0x8000_0000_0000_0000
    var _: UInt = 0xffff_ffff_ffff_ffff

    var _: UInt = -1 // expected-error {{negative integer '-1' overflows when stored into unsigned type 'UInt'}}

    var _: UInt = -0xffff_ffff_ffff_ffff // expected-error {{negative integer '-18446744073709551615' overflows when stored into unsigned type 'UInt'}}
  }

  do {
    // Addition.
    var _: UInt = 0 + 0 // OK
    var _: UInt = 0xffff_ffff_ffff_ffff + 0 // OK
    var _: UInt = 0xffff_ffff_ffff_ffff + 1 // expected-error {{arithmetic operation '18446744073709551615 + 1' (on type 'UInt') results in an overflow}}

    var _: UInt = 0xffff_ffff_ffff_fffe + 1 // OK
    var _: UInt = 0xffff_ffff_ffff_fffe + 2 // expected-error {{arithmetic operation '18446744073709551614 + 2' (on type 'UInt') results in an overflow}}
  }

  do {
    // Subtraction.
    var _: UInt = 0xffff_ffff_ffff_fffe - 1 // OK
    var _: UInt = 0xffff_ffff_ffff_fffe - 0xffff_ffff_ffff_ffff // expected-error {{arithmetic operation '18446744073709551614 - 18446744073709551615' (on type 'UInt') results in an overflow}}

    var _: UInt = 0 - 0 // OK
    var _: UInt = 0 - 1 // expected-error {{arithmetic operation '0 - 1' (on type 'UInt') results in an overflow}}
  }

  do {
    // Multiplication.
    var _: UInt = 0xffff_ffff_ffff_ffff * 0 // OK
    var _: UInt = 0xffff_ffff_ffff_ffff * 1 // OK
    var _: UInt = 0xffff_ffff_ffff_ffff * 2 // expected-error {{arithmetic operation '18446744073709551615 * 2' (on type 'UInt') results in an overflow}}

    var _: UInt = 0xffff_ffff_ffff_ffff * 0xffff_ffff_ffff_ffff // expected-error {{arithmetic operation '18446744073709551615 * 18446744073709551615' (on type 'UInt') results in an overflow}}

    var _: UInt = 0x7fff_ffff_ffff_fffe * 0 // OK
    var _: UInt = 0x7fff_ffff_ffff_fffe * 1 // OK
    var _: UInt = 0x7fff_ffff_ffff_fffe * 2 // OK
    var _: UInt = 0x7fff_ffff_ffff_fffe * 3 // expected-error {{arithmetic operation '9223372036854775806 * 3' (on type 'UInt') results in an overflow}}
  }

  do {
    // Division.
    var _: UInt = 0x7fff_ffff_ffff_fffe / 2 // OK
    var _: UInt = 0x7fff_ffff_ffff_fffe / 0 // expected-error {{division by zero}}

    var _: UInt = 0xffff_ffff_ffff_ffff / 2 // OK
    var _: UInt = 0xffff_ffff_ffff_ffff / 0 // expected-error {{division by zero}}

  }

  do {
    // Remainder.
    var _: UInt = 0x7fff_ffff_ffff_fffe % 2 // OK
    var _: UInt = 0x7fff_ffff_ffff_fffe % 0 // expected-error {{division by zero}}

    var _: UInt = 0xffff_ffff_ffff_ffff % 2 // OK
    var _: UInt = 0xffff_ffff_ffff_ffff % 0 // expected-error {{division by zero}}
  }

  do {
    // Shift operations don't result in overflow errors but note that
    // one cannot use negative values while initializing of an unsigned value
    var _: UInt = 0 >> 0
    var _: UInt = 0 >> 1

    var _: UInt = 123 >> 0
    var _: UInt = 123 >> 1

    var _: UInt = (-1) >> 0  // expected-error {{negative integer '-1' overflows when stored into unsigned type 'UInt'}}
    var _: UInt = (-1) >> 1  // expected-error {{negative integer '-1' overflows when stored into unsigned type 'UInt'}}

    var _: UInt = 0x7fff_ffff_ffff_ffff >> 63
    var _: UInt = 0x7fff_ffff_ffff_ffff >> 64
    var _: UInt = 0x7fff_ffff_ffff_ffff >> 65
  }

  do {
    // Shift operations don't result in overflow errors but note that
    // one cannot use negative values while initializing of an unsigned value
    var _: UInt = 0 << 0
    var _: UInt = 0 << 1

    var _: UInt = 123 << 0
    var _: UInt = 123 << 1

    var _: UInt = (-1) << 0  // expected-error {{negative integer '-1' overflows when stored into unsigned type 'UInt'}}
    var _: UInt = (-1) << 1  // expected-error {{negative integer '-1' overflows when stored into unsigned type 'UInt'}}

    var _: UInt = 0x7fff_ffff_ffff_ffff << 63
    var _: UInt = 0x7fff_ffff_ffff_ffff << 64
    var _: UInt = 0x7fff_ffff_ffff_ffff << 65
  }

  do {
    // bitwise operations. No overflows can happen during these operations
    var _ : UInt = ~0
    var _ : UInt = (0x7fff_ffff_ffff_ffff) | (0x4000_0000_0000_0000 << 1)
    var _ : UInt = (0x7fff_ffff) | 0x8000_0000_0000_0000
  }
}
