// RUN: not %target-swift-frontend -primary-file %s -O -emit-ir -o /dev/null 2>&1 | %FileCheck %s

@_silgen_name("blackHole")
func blackHole(_ value: UnsafeMutableRawPointer?) -> Void

withUnsafeTemporaryAllocation(byteCount: 1, alignment: 3) { buffer in
    blackHole(buffer.baseAddress)
}
// CHECK: error: alignment value must be a power of two
