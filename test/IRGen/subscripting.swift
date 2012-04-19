// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

import swift

struct X {
  value : Int
  
  subscript (i : Int) -> Int {
    // CHECK: define void @_TN12subscripting1X__subgetfRS0_FT1iNSs5Int643valS1__T_
    get { return value + i }
    // CHECK: define i64 @_TN12subscripting1X__subsetfRS0_FT1iNSs5Int64_S1_
    set (val) { value = val - i }
  }
}
