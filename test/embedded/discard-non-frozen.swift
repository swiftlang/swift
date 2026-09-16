// RUN: %target-swift-frontend %s -module-name main -parse-as-library -enable-experimental-feature Embedded -emit-sil -verify -o - | %FileCheck %s

// REQUIRES: swift_feature_Embedded

public struct S<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>

  public init() { p = .allocate(capacity: 1) }

  // Take the pointer out before consuming self, then skip the deinit.
  public consuming func giveUp() -> UnsafeMutablePointer<Int> {
    let q = p
    discard self
    return q
  }

  deinit { p.deallocate() }
}

// A non-generic type in the same position.
public struct T: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  public init() { p = .allocate(capacity: 1) }
  public consuming func giveUp() -> UnsafeMutablePointer<Int> {
    let q = p
    discard self
    return q
  }
  deinit { p.deallocate() }
}

// `discard` skips the deinit, so neither call site destroys the value.
// CHECK-LABEL: sil{{.*}}@$e4main2goyyF
// CHECK-NOT: fD
// CHECK: end sil function
public func go() {
  let q = S<Int>().giveUp()
  q.deallocate()
  let r = T().giveUp()
  r.deallocate()
}
