// RUN: %target-run-simple-swift(-enable-experimental-feature Extern -enable-experimental-feature Embedded -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern

@_extern(c)
func puts(_ string: UnsafePointer<CChar>?) -> CInt

func foo(_ string: UnsafePointer<CChar>?) {
  puts(string)
}

foo("hello")
// CHECK: hello
