// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
func markUsed<T>(t: T) {}

public func f(i : Int?)
{
  // CHECK: !DILocalVariable(name: "gv", {{.*}}line: [[@LINE+1]]
  guard var gv = i else { return }
  markUsed(gv)
}
