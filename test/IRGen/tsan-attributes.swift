// This test verifies that we add the function attributes used by TSan.

// RUN: not %target-swift-frontend -emit-ir -sanitize=thread %s 2>&1| FileCheck %s -check-prefix=TSAN

// TSan is currently only supported on 64 bit mac and simulators.
// (We do not test the simulators here.)
// REQUIRES: CPU=x86_64, OS=macosx

func test() {
}

// TSAN: argument '-sanitize=thread' is not supported on the Swift 2.3 toolchain. You will need to migrate your project to Swift 3 to use this feature.
