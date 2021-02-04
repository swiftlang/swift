// RUN: %target-swift-frontend -primary-file %s -emit-ir -o - | %FileCheck %s

// Marker protocols should have no ABI impact at all, so this source file checks
// for the absence of symbols related to marker protocols.

// CHECK-NOT: $s15marker_protocol1PP
// CHECK-NOT: $s15marker_protocol1PMp

@_marker public protocol P { }

extension Int: P { }
extension Array: P where Element: P { }

// Note: no witness tables
// CHECK: swiftcc void @"$s15marker_protocol7genericyyxAA1PRzlF"(%swift.opaque* noalias nocapture %0, %swift.type* %T)
public func generic<T: P>(_: T) { }

public func testGeneric(i: Int, array: [Int]) {
  generic(i)
  generic(array)
}

public protocol Q: P { }
