// RUN: not %target-swift-frontend -primary-file %s -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s

@_silgen_name("blackHole")
func blackHole(_ value: UnsafeMutableRawPointer?) -> Void

withUnsafeTemporaryAllocation(of: Int.self, capacity: .max) { buffer in
    blackHole(buffer.baseAddress)
}
// CHECK: error: allocation byte count too large
