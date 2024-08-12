// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=windows-msvc

// This file has CRT-specific C stdlib tests, that use
// some APIs present only in CRT.

import CRT

func complexFunctionsAvailableInSwift() {
  let complexValue = _Cbuild(1.0, 2.0) // Construct a complex double using MSVC-specific API.
  let _ = creal(complexValue)
}
