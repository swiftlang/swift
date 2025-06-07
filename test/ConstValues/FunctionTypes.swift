// Constant globals on function types / function pointers
// REQUIRES: swift_feature_CompileTimeValues
// REQUIRES: swift_feature_CompileTimeValuesPreview
// RUN: %target-swift-frontend -emit-ir -primary-file %s -parse-as-library -enable-experimental-feature CompileTimeValues -enable-experimental-feature CompileTimeValuesPreview

func foo_void_to_void() {}
func foo_int_to_int(x: Int) -> Int { return 42 }

@const let constGlobalA1: ()->() = { }
@const let constGlobalA2: @convention(c) ()->() = { }
@const let constGlobalA3: @convention(thin) ()->() = { }

@const let constGlobalB1: ()->() = foo_void_to_void
@const let constGlobalB2: @convention(c) ()->() = foo_void_to_void
@const let constGlobalB3: @convention(thin) ()->() = foo_void_to_void

@const let constGlobalC1: (Int)->(Int) = { _ in return 42 }
@const let constGlobalC2: @convention(c) (Int)->(Int) = { _ in return 42 }
@const let constGlobalC3: @convention(thin) (Int)->(Int) = { _ in return 42 }

@const let constGlobalD1: (Int)->(Int) = foo_int_to_int
@const let constGlobalD2: @convention(c) (Int)->(Int) = foo_int_to_int
@const let constGlobalD3: @convention(thin) (Int)->(Int) = foo_int_to_int
