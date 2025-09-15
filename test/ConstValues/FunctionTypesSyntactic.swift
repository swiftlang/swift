// Constant globals on function types / function pointers
// REQUIRES: swift_feature_CompileTimeValues
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -verify

func foo_void_to_void() {}
func foo_int_to_int(x: Int) -> Int { return 42 }

@const let constGlobalA1: ()->() = { }
// expected-error@-1{{closures not supported in a '@const' expression}}
@const let constGlobalA2: @convention(c) ()->() = { }
// expected-error@-1{{closures not supported in a '@const' expression}}
@const let constGlobalA3: @convention(thin) ()->() = { }
// expected-error@-1{{only 'convention(c)' function values are supported in a '@const' expression}}

@const let constGlobalB1: ()->() = foo_void_to_void // TODO: Diagnose the type of the variable as not eligigle for '@const' (not the init expression)
@const let constGlobalB2: @convention(c) ()->() = foo_void_to_void
@const let constGlobalB3: @convention(thin) ()->() = foo_void_to_void
// expected-error@-1{{only 'convention(c)' function values are supported in a '@const' expression}}

@const let constGlobalC1: (Int)->(Int) = { _ in return 42 }
// expected-error@-1{{closures not supported in a '@const' expression}}
@const let constGlobalC2: @convention(c) (Int)->(Int) = { _ in return 42 }
// expected-error@-1{{closures not supported in a '@const' expression}}
@const let constGlobalC3: @convention(thin) (Int)->(Int) = { _ in return 42 }
// expected-error@-1{{only 'convention(c)' function values are supported in a '@const' expression}}

@const let constGlobalD1: (Int)->(Int) = foo_int_to_int
@const let constGlobalD2: @convention(c) (Int)->(Int) = foo_int_to_int
@const let constGlobalD3: @convention(thin) (Int)->(Int) = foo_int_to_int
// expected-error@-1{{only 'convention(c)' function values are supported in a '@const' expression}}
