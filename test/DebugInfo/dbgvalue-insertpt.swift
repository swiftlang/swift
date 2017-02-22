// RUN: %target-swift-frontend -g -emit-ir %s | %FileCheck %s
for i in 0 ..< 3 {
  // CHECK: %[[ALLOCA:[0-9]+]] = alloca %TSiSg
  // CHECK: %[[CAST:[0-9]+]] = bitcast %TSiSg* %[[ALLOCA]] to i{{32|64}}*
  // CHECK: %[[LD:[0-9]+]] = load i{{32|64}}, i{{32|64}}* %[[CAST]]
  // CHECK-NEXT: call void @llvm.dbg.value(metadata i{{32|64}} %[[LD]],
  // CHECK-SAME:                           metadata ![[I:[0-9]+]],
  // CHECK: ![[I]] = !DILocalVariable(name: "i",
}
