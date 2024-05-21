// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -throws-as-traps | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu || OS=windows-msvc

public func test() {
  withUnsafeTemporaryAllocation(byteCount: MemoryLayout<Int>.size, alignment: MemoryLayout<Int>.alignment) { p in
    p[0] = 42
  }
}

// CHECK: swift_willThrow
