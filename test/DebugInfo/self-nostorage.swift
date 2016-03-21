// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | FileCheck %s

public struct S {
  func f() {
    // CHECK: define {{.*}}_TFV4main1S1ffT_T_
    // CHECK: call void @llvm.dbg.declare(metadata %V4main1S undef,
    // CHECK-SAME:                        metadata ![[SELF:[0-9]+]]
    // CHECK: ![[SELF]] = !DILocalVariable(name: "self", arg: 1,
  }
}
