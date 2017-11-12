// This test verifies that we add the function attributes used by ASan.

// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -sanitize=address %s | %FileCheck %s -check-prefix=ASAN

func test() {
}

// ASAN: Function Attrs: sanitize_address
