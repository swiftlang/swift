// Constant globals on function types / function pointers

// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library

func foo_void_to_void() {}
func foo_int_to_int(x: Int) -> Int { return 42 }

_const let constGlobalA1: ()->() = { }
_const let constGlobalA2: @convention(c) ()->() = { }
_const let constGlobalA3: @convention(thin) ()->() = { }

_const let constGlobalB1: ()->() = foo_void_to_void
_const let constGlobalB2: @convention(c) ()->() = foo_void_to_void
_const let constGlobalB3: @convention(thin) ()->() = foo_void_to_void

_const let constGlobalC1: (Int)->(Int) = { _ in return 42 }
_const let constGlobalC2: @convention(c) (Int)->(Int) = { _ in return 42 }
_const let constGlobalC3: @convention(thin) (Int)->(Int) = { _ in return 42 }

_const let constGlobalD1: (Int)->(Int) = foo_int_to_int
_const let constGlobalD2: @convention(c) (Int)->(Int) = foo_int_to_int
_const let constGlobalD3: @convention(thin) (Int)->(Int) = foo_int_to_int
