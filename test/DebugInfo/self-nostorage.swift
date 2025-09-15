// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

public struct S {
  func f() {
    // CHECK: define {{.*}}$s4main1SV1fyyF
    // CHECK: #dbg_value(i{{.*}} 0,
    // CHECK-SAME:                      ![[SELF:[0-9]+]]
    // CHECK: ![[SELF]] = !DILocalVariable(name: "self", arg: 1,
  }
}
