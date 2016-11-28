// RUN: %target-typecheck-verify-swift -parse-stdlib

precedencegroup AssignmentPrecedence { assignment: true }

var word: Builtin.Word
var i16: Builtin.Int16
var i32: Builtin.Int32
var i64: Builtin.Int64
var i128: Builtin.Int128

// Check that trunc/?ext operations are appropriately available given the
// abstract range of potential Word sizes.

word = Builtin.truncOrBitCast_Int128_Word(i128)
word = Builtin.truncOrBitCast_Int64_Word(i64)
word = Builtin.truncOrBitCast_Int32_Word(i32) // expected-error{{}}
word = Builtin.truncOrBitCast_Int16_Word(i16) // expected-error{{}}

i16 = Builtin.truncOrBitCast_Word_Int16(word)
i32 = Builtin.truncOrBitCast_Word_Int32(word)
i64 = Builtin.truncOrBitCast_Word_Int64(word) // expected-error{{}}
i128 = Builtin.truncOrBitCast_Word_Int128(word) // expected-error{{}}

word = Builtin.zextOrBitCast_Int128_Word(i128) // expected-error{{}}
word = Builtin.zextOrBitCast_Int64_Word(i64) // expected-error{{}}
word = Builtin.zextOrBitCast_Int32_Word(i32)
word = Builtin.zextOrBitCast_Int16_Word(i16)

i16 = Builtin.zextOrBitCast_Word_Int16(word) // expected-error{{}}
i32 = Builtin.zextOrBitCast_Word_Int32(word) // expected-error{{}}
i64 = Builtin.zextOrBitCast_Word_Int64(word)
i128 = Builtin.zextOrBitCast_Word_Int128(word)

word = Builtin.trunc_Int128_Word(i128)
word = Builtin.trunc_Int64_Word(i64) // expected-error{{}}
word = Builtin.trunc_Int32_Word(i32) // expected-error{{}}
word = Builtin.trunc_Int16_Word(i16) // expected-error{{}}

i16 = Builtin.trunc_Word_Int16(word)
i32 = Builtin.trunc_Word_Int32(word) // expected-error{{}}
i64 = Builtin.trunc_Word_Int64(word) // expected-error{{}}
i128 = Builtin.trunc_Word_Int128(word) // expected-error{{}}

word = Builtin.zext_Int128_Word(i128) // expected-error{{}}
word = Builtin.zext_Int64_Word(i64) // expected-error{{}}
word = Builtin.zext_Int32_Word(i32) // expected-error{{}}
word = Builtin.zext_Int16_Word(i16)

i16 = Builtin.zext_Word_Int16(word) // expected-error{{}}
i32 = Builtin.zext_Word_Int32(word) // expected-error{{}}
i64 = Builtin.zext_Word_Int64(word) // expected-error{{}}
i128 = Builtin.zext_Word_Int128(word)
