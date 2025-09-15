// RUN: %target-swift-frontend -swift-version 6 -verify %s -emit-sil -o /dev/null

// REQUIRES: OS=macosx
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

import os.log

let logger = Logger(subsystem: "x.y.z", category: "c")

// Check that this compiles without errors

func testit(buffer: UnsafeRawBufferPointer) {
  logger.fault("buffer \(buffer, privacy: .sensitive)")
}  
