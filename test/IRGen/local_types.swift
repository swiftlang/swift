// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-module %S/Inputs/local_types_helper.swift -o %t
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -parse-as-library %s -I %t > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck -check-prefix=NEGATIVE %s < %t.ll

// XFAIL: linux

import local_types_helper

public func singleFunc() {
  // CHECK-DAG: @_T011local_types10singleFuncyyF06SingleD6StructL_VWV = hidden constant
  struct SingleFuncStruct {
    let i: Int
  }
}

public let singleClosure: () -> () = {
  // CHECK-DAG: @_T011local_types13singleClosureyycvfiyycfU_06SingleD6StructL_VWV = hidden constant
  struct SingleClosureStruct {
    let i: Int
  }
}

public struct PatternStruct {
  public var singlePattern: Int = ({
    // CHECK-DAG: @_T011local_types13PatternStructV06singleC0SivfiSiycfU_06SinglecD0L_VWV = hidden constant
    struct SinglePatternStruct {
      let i: Int
    }
    return 1
  })()
}

public func singleDefaultArgument(i: Int = {
  // CHECK-DAG: @_T011local_types21singleDefaultArgumentySi1i_tFfA_SiycfU_06SingledE6StructL_VWV = hidden constant
  struct SingleDefaultArgumentStruct {
    let i: Int
  }
  return 2

  }()){
    print(i)
}


#if COMPILED_OUT
public func topLevelIfConfig() {
  class LocalClassDisabled {}
}
#else
public func topLevelIfConfig() {
  // CHECK-DAG: @_T011local_types16topLevelIfConfigyyF17LocalClassEnabledL_CMm = hidden global %objc_class
  class LocalClassEnabled {}
}
#endif

public struct NominalIfConfig {
  #if COMPILED_OUT
  public func method() {
    class LocalClassDisabled {}
  }
  #else
  public func method() {
    // CHECK-DAG: @_T011local_types15NominalIfConfigV6methodyyF17LocalClassEnabledL_CMm = hidden global %objc_class
    class LocalClassEnabled {}
  }
  #endif
}

public func innerIfConfig() {
  #if COMPILED_OUT
  class LocalClassDisabled {}
  func inner() {
    class LocalClassDisabled {}
  }
  #else
  // CHECK-DAG: @_T011local_types13innerIfConfigyyF17LocalClassEnabledL_CMm = hidden global %objc_class
  class LocalClassEnabled {}
  func inner() {
    // CHECK-DAG: @_T011local_types13innerIfConfigyyF0C0L0_yyF17LocalClassEnabledL_CMm = hidden global %objc_class
    class LocalClassEnabled {}
  }
  #endif
}


// CHECK-LABEL: define{{( protected)?}} swiftcc void @_T011local_types8callTestyyF() {{.*}} {
public func callTest() {
  test()
} // CHECK: {{^[}]$}}

// NEGATIVE-NOT: LocalClassDisabled
