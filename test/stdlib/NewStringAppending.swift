// RUN: %target-run-stdlib-swift | %FileCheck %s
// REQUIRES: executable_test
//
// Parts of this test depend on memory allocator specifics.  The test
// should be rewritten soon so it doesn't expose legacy components
// like OpaqueString anyway, so we can just disable the failing
// configuration
//
// Memory allocator specifics also vary across platforms.
// REQUIRES: CPU=x86_64, OS=macosx

import Foundation
import Swift

// ===------- Appending -------===

// Michael NOTE: This test was testing the properties of _StringCore and how it
// changed through appending, which we are replacing and currently have gutted.
// I believe we should replace each of these with internal checks for the Swift
// 4-era representation.

// CHECK: --- Appending ---
print("--- Appending ---")
