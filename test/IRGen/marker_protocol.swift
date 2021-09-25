// RUN: %target-swift-frontend -primary-file %s -emit-ir -o - | %FileCheck %s

// Marker protocols should have no ABI impact at all, so this source file checks
// for the absence of symbols related to marker protocols.

// CHECK-NOT: $s15marker_protocol1PP
// CHECK-NOT: $s15marker_protocol1PMp

// REQUIRES: PTRSIZE=64

@_marker public protocol P { }

extension Int: P { }
extension Array: P where Element: P { }

// CHECK: @"$s15marker_protocol1QMp" = {{(dllexport |protected )?}}constant
// CHECK-SAME: i32 trunc{{.*}}s15marker_protocolMXM{{.*}}s15marker_protocol1QMp
// CHECK-SAME: i32 0, i32 5, i32 0
public protocol Q: P {
  func f()
  func g()
  func h()
  func i()
  func j()
}

// Note: no witness tables
// CHECK: swiftcc void @"$s15marker_protocol7genericyyxAA1PRzlF"(%swift.opaque* noalias nocapture %0, %swift.type* %T)
public func generic<T: P>(_: T) { }

public func testGeneric(i: Int, array: [Int]) {
  generic(i)
  generic(array)
}

// Forming an existential involving a marker protocol would crash the compiler
protocol SelfConstrainedProtocol {
  static var shared: Self { get }
}

struct Foo: SelfConstrainedProtocol {
  let x: P
  static var shared: Self {
    Foo(x: 123)
  }
}
