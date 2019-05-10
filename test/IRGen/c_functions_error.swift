// RUN: not %target-swift-frontend -Xcc -mno-sse -import-objc-header %S/Inputs/c_functions_error.h -primary-file %s -emit-ir

// This should fail, but not crash.

// REQUIRES: CPU=x86_64

func call_buggy_builtin() {
  produce_error_in_clang_irgen();
}
