// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-module %S/Inputs/local_types_helper.swift -o %t
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -enable-objc-interop -emit-ir -parse-as-library %s -I %t | %FileCheck -check-prefix CHECK -check-prefix NEGATIVE %s

import local_types_helper

public func singleFunc() {
  // CHECK-DAG: @"$s11local_types10singleFuncyyF06SingleD6StructL_VMf" = internal constant
  struct SingleFuncStruct {
    let i: Int
  }
}

public let singleClosure: () -> () = {
  // CHECK-DAG: @"$s11local_types13singleClosureyycvpfiyycfU_06SingleD6StructL_VMf" = internal constant
  struct SingleClosureStruct {
    let i: Int
  }
}

public struct PatternStruct {
  public var singlePattern: Int = ({
    // CHECK-DAG: @"$s11local_types13PatternStructV06singleC0SivpfiSiyXEfU_06SinglecD0L_VMf" = internal constant
    struct SinglePatternStruct {
      let i: Int
    }
    return 1
  })()
}

#if COMPILED_OUT
public func topLevelIfConfig() {
  class LocalClassDisabled {}
}
#else
public func topLevelIfConfig() {
  // CHECK-DAG: @"$s11local_types16topLevelIfConfigyyF17LocalClassEnabledL_CMm" = internal global %objc_class
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
    // CHECK-DAG: @"$s11local_types15NominalIfConfigV6methodyyF17LocalClassEnabledL_CMm" = internal global %objc_class
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
  // CHECK-DAG: @"$s11local_types13innerIfConfigyyF17LocalClassEnabledL_CMm" = internal global %objc_class
  class LocalClassEnabled {}
  func inner() {
    // CHECK-DAG: @"$s11local_types13innerIfConfigyyF0C0L0_yyF17LocalClassEnabledL_CMm" = internal global %objc_class
    class LocalClassEnabled {}
  }
  #endif
}


// CHECK-LABEL: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s11local_types8callTestyyF"() {{.*}} {
public func callTest() {
  test()
} // CHECK: {{^[}]$}}

// NEGATIVE-NOT: LocalClassDisabled
