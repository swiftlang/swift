// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s --check-prefix=CHECK2
func use<T>(_ t: T) {}

public func f(_ i : Int?)
{
  // CHECK: define {{.*}}@_T04main1fySiSgF
  // The shadow copy store should not have a location.
  // CHECK: @llvm.dbg.declare(metadata {{(i32|i64)}}* %val.addr, {{.*}}, !dbg ![[DBG1:.*]]
  // CHECK: %[[PHI:.*]] = phi
  // CHECK: store {{(i32|i64)}} %[[PHI]], {{(i32|i64)}}* %val.addr, align {{(4|8)}}, !dbg ![[DBG0:.*]]
  // CHECK: ![[F:.*]] = distinct !DISubprogram(name: "f",
  // CHECK: ![[DBG1]] = !DILocation(line: [[@LINE+2]],
  // CHECK: ![[DBG0]] = !DILocation(line: 0, scope: ![[F]])
  guard let val = i else { return }
  use(val)
}

public func g(_ s : String?)
{
  // CHECK2: define {{.*}}@_T04main1gySSSgF
  // The shadow copy store should not have a location.
  // CHECK2: getelementptr inbounds {{.*}} %s.debug, {{.*}}, !dbg ![[DBG0:.*]]
  // CHECK2: ![[G:.*]] = distinct !DISubprogram(name: "g",
  // CHECK2: ![[DBG0]] = !DILocation(line: 0, scope: ![[G]])
  guard let val = s else { return }
  use(val)
}
