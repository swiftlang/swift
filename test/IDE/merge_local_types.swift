// Tests merging of modules each with their own local types

// RUN: rm -rf %t && mkdir %t

// Create separate modules and merge them together
// RUN: %target-swiftc_driver -v -emit-module -module-name LocalTypesMerged -o %t/LocalTypesMerged.swiftmodule %s %S/local_types.swift

// RUN: %target-swift-ide-test -print-local-types -I %t -module-to-print LocalTypesMerged -source-filename %s | FileCheck %s

public func toMerge() {
  // CHECK-DAG: VF16LocalTypesMerged7toMergeFT_T_L_16SingleFuncStruct
  struct SingleFuncStruct {
    let sfsi: Int
  }
  // CHECK-DAG: CF16LocalTypesMerged7toMergeFT_T_L_15SingleFuncClass
  class SingleFuncClass {
    let sfcs: String
    init(s: String) {
      self.sfcs = s
    }
  }
  // CHECK-DAG: OF16LocalTypesMerged7toMergeFT_T_L_14SingleFuncEnum
  enum SingleFuncEnum {
    case SFEI(Int)
  }
}

// Merged from local_types.swift

//CHECK-DAG: OF16LocalTypesMerged7toMergeFT_T_L_14SingleFuncEnum
//CHECK-DAG: OF16LocalTypesMerged10singleFuncFT_T_L_14SingleFuncEnum
//CHECK-DAG: CFIF16LocalTypesMerged21singleDefaultArgumentFT1iSi_T_A_U_FT_SiL_26SingleDefaultArgumentClass
//CHECK-DAG: OF16LocalTypesMergedg13singlePatternSiL_17SinglePatternEnum
//CHECK-DAG: CFIv16LocalTypesMerged13singleClosureFT_T_iU_FT_T_L_18SingleClosureClass
//CHECK-DAG: CFF16LocalTypesMerged10doubleFuncFT_T_L_9innerFuncFT_T_L_15DoubleFuncClass
//CHECK-DAG: VFIv16LocalTypesMerged13singleClosureFT_T_iU_FT_T_L_19SingleClosureStruct
//CHECK-DAG: OFIF16LocalTypesMerged21singleDefaultArgumentFT1iSi_T_A_U_FT_SiL_25SingleDefaultArgumentEnum
//CHECK-DAG: OFIv16LocalTypesMerged13singleClosureFT_T_iU_FT_T_L_17SingleClosureEnum
//CHECK-DAG: CF16LocalTypesMerged10singleFuncFT_T_L_15SingleFuncClass
//CHECK-DAG: CF16LocalTypesMergedg13singlePatternSiL_18SinglePatternClass
//CHECK-DAG: VF16LocalTypesMerged15transparentFuncFT_T_L_21TransparentFuncStruct
//CHECK-DAG: CFFIv16LocalTypesMerged13doubleClosureFT_T_iU_FT_T_U_FT_T_L_18DoubleClosureClass
//CHECK-DAG: VFFIv16LocalTypesMerged13doubleClosureFT_T_iU_FT_T_U_FT_T_L_19DoubleClosureStruct
//CHECK-DAG: VFIF16LocalTypesMerged21singleDefaultArgumentFT1iSi_T_A_U_FT_SiL_27SingleDefaultArgumentStruct
//CHECK-DAG: OFFIv16LocalTypesMerged13doubleClosureFT_T_iU_FT_T_U_FT_T_L_17DoubleClosureEnum
//CHECK-DAG: VFF16LocalTypesMerged10doubleFuncFT_T_L_9innerFuncFT_T_L_16DoubleFuncStruct
//CHECK-DAG: OF16LocalTypesMerged15transparentFuncFT_T_L_19TransparentFuncEnum
//CHECK-DAG: OFF16LocalTypesMerged10doubleFuncFT_T_L_9innerFuncFT_T_L_14DoubleFuncEnum
//CHECK-DAG: CF16LocalTypesMerged7toMergeFT_T_L_15SingleFuncClass
//CHECK-DAG: VF16LocalTypesMerged7toMergeFT_T_L_16SingleFuncStruct
//CHECK-DAG: CF16LocalTypesMerged15transparentFuncFT_T_L_20TransparentFuncClass
//CHECK-DAG: VF16LocalTypesMerged10singleFuncFT_T_L_16SingleFuncStruct
//CHECK-DAG: VF16LocalTypesMergedg13singlePatternSiL_19SinglePatternStruct
