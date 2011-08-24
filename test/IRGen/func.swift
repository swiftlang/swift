// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

import swift

func f0(x : int) -> int {
  return 0
}

// CHECK: declare i64 @f0(i64)
