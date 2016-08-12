// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
func markUsed<T>(_ t: T) {}

public func f(_ s : String?)
{
  if var str = s {
    // CHECK: !DILocalVariable(name: "str", {{.*}}line: [[@LINE-1]]
    str = "foo"
    markUsed(str)
  }
}
