// This test verifies that we add the function attributes used by TSan.

// RUN: %target-swift-frontend -emit-ir -sanitize=thread %s | %FileCheck %s -check-prefix=TSAN

// TSan is currently only supported on 64 bit mac and simulators.
// (We do not test the simulators here.)
// REQUIRES: CPU=x86_64, OS=macosx

func test() {
}

// TSAN: Function Attrs: sanitize_thread
