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
  // CHECK: define {{.*}}void @"$S8ErrorVar6simpleyys5Int64VKF"(i64, %swift.refcounted* swiftself, %swift.error** noalias nocapture dereferenceable(4))
  // CHECK: call void @llvm.dbg.declare
  // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[ERROR:[0-9]+]], metadata !DIExpression(DW_OP_deref))
  // CHECK: ![[ERRTY:.*]] = !DICompositeType({{.*}}identifier: "$Ss5Error_pD"
  // CHECK: ![[ERROR]] = !DILocalVariable(name: "$error", arg: 2,
  // CHECK-SAME:                          type: ![[ERRTY]],
  // CHECK-SAME:                          flags: DIFlagArtificial)
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
