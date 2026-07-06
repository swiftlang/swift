// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -module-name main -O %s -emit-ir | %FileCheck %s --check-prefix=CHECK-IR
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -module-name main -O %s -c -o %t/a.o
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/a.o %target-embedded-posix-shim -o %t/a.out -dead_strip
// RUN: %llvm-nm --defined-only --format=just-symbols --demangle %t/a.out | sort | %FileCheck %s --check-prefix=CHECK-NM
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded

// UNSUPPORTED: OS=emscripten
// With `-o a.out`, emcc emits a JS launcher named `a.out` plus the wasm binary
// at `a.out.wasm`, so the CHECK-NM step's `llvm-nm a.out` rejects the JS file as
// "not recognized as a valid object file".

public func a_this_is_unused() { }

@used
public func b_this_is_unused_but_explicitly_retained() { }

// CHECK-IR: define {{.*}}@"$e4main16a_this_is_unusedyyF"()
// CHECK-IR: define {{.*}}@"$e4main40b_this_is_unused_but_explicitly_retainedyyF"()

// CHECK-NM-NOT: $e4main14this_is_unusedyyF
// CHECK-NM: $e4main40b_this_is_unused_but_explicitly_retainedyyF

print("Hello")
// CHECK: Hello
