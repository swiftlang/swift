// This test verifies that we add the function attributes used by sanitizers.

// RUN: %target-swift-frontend -emit-ir -sanitize=address %s | FileCheck %s -check-prefix=ASAN
// RUN: %target-swift-frontend -emit-ir -target x86_64-apple-macosx10.9 -sanitize=thread %s | FileCheck %s -check-prefix=TSAN

// XFAIL: linux

func test() {
}

// ASAN: Function Attrs: sanitize_address
// TSAN: Function Attrs: sanitize_thread
