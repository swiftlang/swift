// RUN: %target-parse-verify-swift %clang-importer-sdk

import ctypes

func testBitfieldMembers() -> StructWithBitfields {
  return StructWithBitfields()
  // TODO: Expose the bitfields as properties.
}

func useStructWithBitfields(mrm: ModRM) -> ModRM {
  // TODO: Make bitfield fields available
  let rm: CUnsignedInt = mrm.rm // expected-error{{}}
  let reg: CUnsignedInt = mrm.reg // expected-error{{}}
  let mod: CUnsignedInt = mrm.mod // expected-error{{}}
  let opcode: CUnsignedInt = mrm.opcode
  return mrm
}

// Incompletely imported structs shouldn't have elementwise initializers.
// They can still be zero-initialized using the default initializer.
func constructStructWithBitfields(x: CUnsignedInt) {
  _ = StructWithBitfields() as StructWithBitfields
  _ = StructWithBitfields(First: x) as StructWithBitfields// expected-error{{}}
  // TODO: Fully import bitfields.
  _ = StructWithBitfields(First: x, Second: x, Third: x) as StructWithBitfields// expected-error{{}}
}
