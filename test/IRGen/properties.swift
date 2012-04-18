// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift
var _x : Int = 0;
var x : Int {
  // CHECK: define i64 @_T10properties__get1xFT_NSs5Int64
  get { return -_x; }
  // CHECK: define void @_T10properties__set1xFT5valueNSs5Int64_T_
  set { _x = value + 1; }
}

func f() ->Int {
  x = 17
  return x
}
