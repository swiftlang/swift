// RUN: %target-typecheck-verify-swift %clang-importer-sdk

import ctypes

func useStructWithBitfields(_ mrm: ModRM) -> ModRM {
  let rm: CUnsignedInt = mrm.rm
  let reg: CUnsignedInt = mrm.reg
  let mod: CUnsignedInt = mrm.mod
  let opcode: CUnsignedInt = mrm.opcode

  var new: ModRM = ModRM()
  new.rm = rm
  new.reg = reg
  new.mod = mod
  new.opcode = opcode

  return mrm
}

func constructStructWithBitfields(_ x: CUnsignedInt) {
  _ = StructWithBitfields()
  _ = StructWithBitfields(First: x, Second: x, Third: x)
}
