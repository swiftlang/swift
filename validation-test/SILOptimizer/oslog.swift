// RUN: %target-swift-frontend -swift-version 6 -O -verify %s -emit-ir -module-name=test | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 -Osize -verify %s -emit-ir -module-name=test | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

import os.log

let logger = Logger(subsystem: "x.y.z", category: "c")

// Check that this compiles without errors or object allocations under -O and
// -Osize.

// rdar://186404026
// CHECK-NOT: @swift_allocObject
// CHECK-NOT: @swift_deallocObject

func testit(buffer: UnsafeRawBufferPointer) {
  logger.fault("buffer \(buffer, privacy: .sensitive)")
}  

@inline(never)
public func interpolate1(_ log: Logger, _ x: Int) {
  log.log("value \(x)")
}

@inline(never)
public func interpolate3(_ log: Logger, _ x: Int, _ y: Int, _ z: Int) {
  log.log("\(x) \(y) \(z)")
}
