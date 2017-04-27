// Tests merging of modules each with their own local types

// RUN: rm -rf %t && mkdir -p %t

// Create separate modules and merge them together
// RUN: %target-swiftc_driver -v -emit-module -module-name LocalTypesMerged -o %t/LocalTypesMerged.swiftmodule %s %S/local_types.swift

// RUN: %target-swift-ide-test -print-local-types -I %t -module-to-print LocalTypesMerged -source-filename %s | %FileCheck %s

public func toMerge() {
  // CHECK-DAG: 16LocalTypesMerged7toMergeyyF16SingleFuncStructL_V
  struct SingleFuncStruct {
    let sfsi: Int
  }
  // CHECK-DAG: 16LocalTypesMerged7toMergeyyF15SingleFuncClassL_C
  class SingleFuncClass {
    let sfcs: String
    init(s: String) {
      self.sfcs = s
    }
  }
  // CHECK-DAG: 16LocalTypesMerged7toMergeyyF14SingleFuncEnumL_O
  enum SingleFuncEnum {
    case SFEI(Int)
  }
}

// Merged from local_types.swift

//CHECK-DAG: 16LocalTypesMerged7toMergeyyF14SingleFuncEnumL_O
//CHECK-DAG: 16LocalTypesMerged10singleFuncyyF06SingleE4EnumL_O
//CHECK-DAG: 16LocalTypesMerged21singleDefaultArgumentySi1i_tFfA_SiycfU_06SingleeF5ClassL_C
//CHECK-DAG: 16LocalTypesMerged13singlePatternSifg06SingleE4EnumL_O
//CHECK-DAG: 16LocalTypesMerged13singleClosureyycvfiyycfU_06SingleE5ClassL_C
//CHECK-DAG: 16LocalTypesMerged10doubleFuncyyF05innerE0L_yyF06DoubleE5ClassL_C
//CHECK-DAG: 16LocalTypesMerged13singleClosureyycvfiyycfU_06SingleE6StructL_V
//CHECK-DAG: 16LocalTypesMerged21singleDefaultArgumentySi1i_tFfA_SiycfU_06SingleeF4EnumL_O
//CHECK-DAG: 16LocalTypesMerged13singleClosureyycvfiyycfU_06SingleE4EnumL_O
//CHECK-DAG: 16LocalTypesMerged10singleFuncyyF06SingleE5ClassL_C
//CHECK-DAG: 16LocalTypesMerged13singlePatternSifg06SingleE5ClassL_C
//CHECK-DAG: 16LocalTypesMerged13doubleClosureyycvfiyycfU_yycfU_06DoubleE5ClassL_C
//CHECK-DAG: 16LocalTypesMerged13doubleClosureyycvfiyycfU_yycfU_06DoubleE6StructL_V
//CHECK-DAG: 16LocalTypesMerged21singleDefaultArgumentySi1i_tFfA_SiycfU_06SingleeF6StructL_V
//CHECK-DAG: 16LocalTypesMerged13doubleClosureyycvfiyycfU_yycfU_06DoubleE4EnumL_O
//CHECK-DAG: 16LocalTypesMerged10doubleFuncyyF05innerE0L_yyF06DoubleE6StructL_V
//CHECK-DAG: 16LocalTypesMerged10doubleFuncyyF05innerE0L_yyF06DoubleE4EnumL_O
//CHECK-DAG: 16LocalTypesMerged7toMergeyyF15SingleFuncClassL_C
//CHECK-DAG: 16LocalTypesMerged7toMergeyyF16SingleFuncStructL_V
//CHECK-DAG: 16LocalTypesMerged10singleFuncyyF06SingleE6StructL_V
//CHECK-DAG: 16LocalTypesMerged13singlePatternSifg06SingleE6StructL_V
