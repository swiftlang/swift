// RUN: %target-swift-frontend -emit-ir -O %s | %FileCheck %s

// REQUIRES: CPU=arm64 || CPU=arm64e || CPU=x86_64

@_nonatomicReferenceCounting
public class Fast {
  var x = 0
}

public final class FastChild: Fast {
  var y = 1
}

public final class Slow {
  var x = 0
}

var sink: [AnyObject] = []
var fastSink: [Fast] = []
var slowSink: [Slow] = []

// References typed as an annotated class (or a subclass of one) are retained
// and released without atomics.
// CHECK-LABEL: define {{.*}}@"$s18nonatomic_class_rc4fastySiAA4FastC_AA0E5ChildCtF"
// CHECK-NOT: call {{.*}}@swift_retain
// CHECK: call {{.*}}@swift_nonatomic_retain
// CHECK-NOT: call {{.*}}@swift_retain
@inline(never)
public func fast(_ a: Fast, _ b: FastChild) -> Int {
  fastSink.append(a)
  fastSink.append(b)
  return a.x &+ b.y
}

// Other classes are unaffected.
// CHECK-LABEL: define {{.*}}@"$s18nonatomic_class_rc4slowySiAA4SlowCF"
// CHECK-NOT: swift_nonatomic
// CHECK: call {{.*}}@swift_retain
@inline(never)
public func slow(_ a: Slow) -> Int {
  slowSink.append(a)
  return a.x
}

// Dropping references from the array releases them non-atomically too.
// CHECK-LABEL: define {{.*}}@"$s18nonatomic_class_rc5dropFyyF"
// CHECK: call {{.*}}@swift_nonatomic_release
@inline(never)
public func dropF() {
  fastSink.removeLast()
}
