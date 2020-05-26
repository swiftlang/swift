// RUN: %target-typecheck-verify-swift
// REQUIRES: OS=windows-msvc

import MSVCRT
typealias JumpBuffer = _JBTYPE

func test_unavailable_returns_twice_function() {
  var x: JumpBuffer
  _ = _setjmp(&x) // expected-error {{'_setjmp' is unavailable in Swift: Functions that return more than once are unavailable in swift}}
}

