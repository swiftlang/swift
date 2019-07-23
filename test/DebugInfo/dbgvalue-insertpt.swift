// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s
// FIXME: This test should be testing a non-shadow-copied value instead.
for i in 0 ..< 3 {
  // CHECK: %[[ALLOCA:[0-9]+]] = alloca %TSiSg
  // CHECK: %i.debug = alloca i{{32|64}}
  // CHECK-NEXT: call void @llvm.dbg.declare(metadata i{{32|64}}* %i.debug,
  // CHECK-SAME:                           metadata ![[I:[0-9]+]],
  // CHECK: call swiftcc{{.*}} @{{.*}}next{{.*}}
  // CHECK: %[[LD:[0-9]+]] = load i{{32|64}}, i{{32|64}}*
  // CHECK: br i1 {{%.*}}, label %[[FAIL:.*]], label %[[SUCCESS:.*]],
  //
  // CHECK: ; <label>:[[SUCCESS]]:
  // CHECK: br label %[[NEXT_BB:.*]],
  //
  // CHECK: ; <label>:[[NEXT_BB]]:
  // CHECK: %[[PHI_VAL:.*]] = phi i{{32|64}} [ %[[LD]], %[[SUCCESS]] ]
  // CHECK: store i{{32|64}} %[[PHI_VAL]], i{{32|64}}* %i.debug
  // CHECK: ![[I]] = !DILocalVariable(name: "i",
}
