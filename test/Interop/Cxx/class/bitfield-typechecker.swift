// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default

import BitFields

func read(_ s: BitFields) -> UInt32 {
  return s.a + s.b + s.c
}

func write(_ s: inout BitFields) {
  s.a = 1
  s.b = 2
  s.c = 3
}
