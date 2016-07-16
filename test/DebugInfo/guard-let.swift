// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
func use<T>(_ t: T) {}

public func f(_ i : Int?)
{
  // CHECK: define {{.*}}@_TF4main1fFGSqSi_T_
  // CHECK: %[[PHI:.*]] = phi
  // The shadow copy store should not have a location.
  // CHECK: store {{(i32|i64)}} %[[PHI]], {{(i32|i64)}}* %val.addr, align {{(4|8)}}, !dbg ![[DBG0:.*]]
  // CHECK: @llvm.dbg.declare(metadata {{(i32|i64)}}* %val.addr, {{.*}}, !dbg ![[DBG1:.*]]
  // CHECK: ![[F:.*]] = distinct !DISubprogram(name: "f",
  // CHECK: ![[DBG0]] = !DILocation(line: 0, scope: ![[F]])
  // CHECK: ![[DBG1]] = !DILocation(line: [[@LINE+1]],
  guard let val = i else { return }
  use(val)
}
