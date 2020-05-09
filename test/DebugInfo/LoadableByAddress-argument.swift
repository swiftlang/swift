// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

public struct Large {
  let field1 : Int64 = 1
  let field2 : Int64 = 2
  let field3 : Int64 = 3
  let field4 : Int64 = 4
  let field5 : Int64 = 5
  let field6 : Int64 = 6
  let field7 : Int64 = 7
  let field8 : Int64 = 8
}

// CHECK: !DILocalVariable(name: "largeArg", arg: 1
public func f(_ largeArg : Large) {}
