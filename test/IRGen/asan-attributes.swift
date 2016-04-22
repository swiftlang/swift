// This test verifies that we add the function attributes used by ASan.

// RUN: %target-swift-frontend -emit-ir -disable-incremental-llvm-codegen -sanitize=address %s | FileCheck %s -check-prefix=ASAN

// XFAIL: linux

func test() {
}

// ASAN: Function Attrs: sanitize_address
