// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -throws-as-traps | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

public func test() {
  withUnsafeTemporaryAllocation(byteCount: MemoryLayout<Int>.size, alignment: MemoryLayout<Int>.alignment) { p in
    p[0] = 42
  }
}

// CHECK-NOT: swift_willThrow
