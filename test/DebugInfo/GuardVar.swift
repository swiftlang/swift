// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
func markUsed<T>(_ t: T) {}

public func f(_ i : Int?)
{
  // CHECK: !DILocalVariable(name: "gv", {{.*}}line: [[@LINE+1]]
  guard var gv = i else { return }
  markUsed(gv)
}
