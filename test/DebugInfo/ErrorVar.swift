// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
// REQUIRES: CPU=i386
class Obj {}

enum MyError : Error {
  case Simple
  case WithObj(Obj)
}

// i386 does not pass swifterror in a register. To support debugging of the
// thrown error we create a shadow stack location holding the address of the
// location that holds the pointer to the error instead.
func simple(_ placeholder: Int64) throws -> () {
  // CHECK: define {{.*}}void @"$s8ErrorVar6simpleyys5Int64VKF"(
  // CHECK-SAME: i64
  // CHECK-SAME: %swift.refcounted* {{.*}}swiftself
  // CHECK-SAME: %swift.error** noalias {{(nocapture|captures\(none\))}} dereferenceable(4)
  // CHECK: #dbg_declare
  // CHECK: #dbg_declare({{.*}}, ![[ERROR:[0-9]+]], !DIExpression(DW_OP_deref)
  // CHECK-DAG: ![[ERRTY:.*]] = !DICompositeType({{.*}}identifier: "$ss5Error_pD"
  // CHECK-DAG: ![[ERROR]] = !DILocalVariable(name: "$error", arg: 2, {{.*}}, type: ![[ERRTY]], flags: DIFlagArtificial)
  throw MyError.Simple
}

func obj() throws -> () {
  throw MyError.WithObj(Obj())
}

public func foo() {
  do {
    try simple(1)
    try obj()
  }
  catch {}
}
