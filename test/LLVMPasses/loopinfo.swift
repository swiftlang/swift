// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-irgen -O -o %t/loopinfo.ll  %s 
// RUN: %swift-llvm-opt -passes='print<loops>' %t/loopinfo.ll 2>&1 | %FileCheck %s

// CHECK: Loop at depth 1 containing
public func iterate1(urbp: UnsafeRawBufferPointer) -> Int {
  var s = 0
  for v in urbp {
    s += Int(v)
  }
  return s
}

// CHECK: Loop at depth 1 containing
public func iterate2(ubp: UnsafeBufferPointer<Int>) -> Int {
  var s = 0
  for v in ubp {
    s += v
  }
  return s
}



