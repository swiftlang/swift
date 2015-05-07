// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
class Obj {}

enum MyError : _ErrorType {
  case Simple
  case WithObj(Obj)
}

func simple(placeholder: Int64) throws -> () {
  // CHECK: define {{.*}}void @_TF6Errors6simpleFzVSs5Int64T_(i64, %swift.refcounted*, %swift.error**)
  // CHECK: call void @llvm.dbg.declare
  // CHECK: call void @llvm.dbg.declare({{.*}}, metadata ![[ERROR:[0-9]+]], metadata ![[DEREF:[0-9]+]])
  // CHECK: ![[ERROR]] = !DILocalVariable(tag: DW_TAG_arg_variable, name: "$error", arg: 2, {{.*}} type: !"_TtPSs10_ErrorType_", flags: DIFlagArtificial)
  // CHECK: ![[DEREF]] = !DIExpression(DW_OP_deref)
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
