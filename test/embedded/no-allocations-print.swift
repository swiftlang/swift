// RUN: %target-swift-emit-ir %s -enable-experimental-feature Embedded -no-allocations

// RUN: %target-swift-emit-ir -target armv7-apple-none-macho -no-allocations %s -enable-experimental-feature Embedded
// RUN: %target-swift-emit-ir -target arm64-apple-none-macho -no-allocations %s -enable-experimental-feature Embedded

// UNSUPPORTED: OS=wasi
// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: CODEGENERATOR=ARM
// REQUIRES: embedded_stdlib_cross_compiling
// REQUIRES: swift_feature_Embedded

print("Hello Embedded Swift!")
print(42)
print(false)
