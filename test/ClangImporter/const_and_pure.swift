// RUN: %target-swift-frontend -emit-sil %s -enable-objc-interop -import-objc-header %S/Inputs/const_and_pure.h | %FileCheck %s

func testit() {
	_ = const_function()

  _ = pure_function()

  _ = normal_function()
}

// CHECK: sil [readnone] [asmname "const_function"] [clang const_function] @$sSo14const_functions5Int32VyFTo : $@convention(c) () -> Int32
// CHECK: sil [readonly] [asmname "pure_function"] [clang pure_function] @$sSo13pure_functions5Int32VyFTo : $@convention(c) () -> Int32
// CHECK: sil [asmname "normal_function"] [clang normal_function] @$sSo15normal_functions5Int32VyFTo : $@convention(c) () -> Int32


