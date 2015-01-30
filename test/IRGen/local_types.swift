// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -emit-module %S/Inputs/local_types_helper.swift -o %t
// RUN: %target-swift-frontend -emit-ir -parse-as-library %s -I %t | FileCheck %s

import local_types_helper

// CHECK-DAG: @_TMdVF18local_types_helper4testFT_T_L_1S = external hidden global %swift.full_type

public func singleFunc() {
  // CHECK-DAG: @_TWVVF11local_types10singleFuncFT_T_L_16SingleFuncStruct = hidden constant
  struct SingleFuncStruct {
    let i: Int
  }
}

public let singleClosure: () -> () = {
  // CHECK-DAG: @_TWVVFIv11local_types13singleClosureFT_T_iU_FT_T_L_19SingleClosureStruct = hidden constant
  struct SingleClosureStruct {
    let i: Int
  }
}

public struct PatternStruct {
  public var singlePattern: Int = ({
    // CHECK-DAG: @_TWVVFIvV11local_types13PatternStruct13singlePatternSiiU_FT_SiL_19SinglePatternStruct = hidden constant
    struct SinglePatternStruct {
      let i: Int
    }
    return 1
  })()
}

public func singleDefaultArgument(i: Int = {
  // CHECK-DAG: @_TWVVFIF11local_types21singleDefaultArgumentFT1iSi_T_A_U_FT_SiL_27SingleDefaultArgumentStruct = hidden constant
  struct SingleDefaultArgumentStruct {
    let i: Int
  }
  return 2

  }()){
    println(i)
}


// CHECK-LABEL: define void @_TF11local_types8callTestFT_T_() {
public func callTest() {
  // CHECK: call %swift.type* @_TMaMVF18local_types_helper4testFT_T_L_1S()
  test()
} // CHECK: {{^[}]$}}
