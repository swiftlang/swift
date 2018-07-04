// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -enable-objc-interop -emit-ir -parse-as-library -swift-version 3 %s | %FileCheck %s

public func singleDefaultArgument(i: Int = {
  // CHECK-DAG: @"$S18local_types_swift321singleDefaultArgument1iySi_tFfA_SiycfU_06SingleeF6StructL_VMf" = internal constant
  struct SingleDefaultArgumentStruct {
    let i: Int
  }
  return 2

  }()){
    print(i)
}


