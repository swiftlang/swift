// Tests lookup and mangling of local types

// RUN: rm -rf %t && mkdir %t
// RUN: %target-swiftc_driver -v -emit-module -module-name LocalTypes -o %t/LocalTypes.swiftmodule %s
// RUN: %target-swift-ide-test -print-local-types -I %t -module-to-print LocalTypes -source-filename %s | FileCheck %s

public func singleFunc() {
  // CHECK-DAG: VF10LocalTypes10singleFuncFT_T_L_16SingleFuncStruct
  struct SingleFuncStruct {
    let sfsi: Int
  }
  // CHECK-DAG: CF10LocalTypes10singleFuncFT_T_L_15SingleFuncClass
  class SingleFuncClass {
    let sfcs: String
    init(s: String) {
      self.sfcs = s
    }
  }
  // CHECK-DAG: OF10LocalTypes10singleFuncFT_T_L_14SingleFuncEnum
  enum SingleFuncEnum {
    case SFEI(Int)
  }
}

public func singleFuncWithDuplicates(fake: Bool) {
  if fake {
    // CHECK-DAG: VF10LocalTypes24singleFuncWithDuplicatesFSbT_L_16SingleFuncStruct
    struct SingleFuncStruct {
      let sfsi: Int
    }
    // CHECK-DAG: CF10LocalTypes24singleFuncWithDuplicatesFSbT_L_15SingleFuncClass
    class SingleFuncClass {
      let sfcs: String
      init(s: String) {
        self.sfcs = s
      }
    }
    // CHECK-DAG: OF10LocalTypes24singleFuncWithDuplicatesFSbT_L_14SingleFuncEnum
    enum SingleFuncEnum {
      case SFEI(Int)
    }
  } else {
    // CHECK-DAG: VF10LocalTypes24singleFuncWithDuplicatesFSbT_L0_16SingleFuncStruct
    struct SingleFuncStruct {
      let sfsi: Int
    }
    // CHECK-DAG: CF10LocalTypes24singleFuncWithDuplicatesFSbT_L0_15SingleFuncClass
    class SingleFuncClass {
      let sfcs: String
      init(s: String) {
        self.sfcs = s
      }
    }
    // CHECK-DAG: OF10LocalTypes24singleFuncWithDuplicatesFSbT_L0_14SingleFuncEnum
    enum SingleFuncEnum {
      case SFEI(Int)
    }
  }
}

public let singleClosure: () -> () = {
  // CHECK-DAG: VF10LocalTypesU_FT_T_L_19SingleClosureStruct
  struct SingleClosureStruct {
    let scsi: Int
  }
  // CHECK-DAG: CF10LocalTypesU_FT_T_L_18SingleClosureClass
  class SingleClosureClass {
    let sccs: String
    init(s: String) {
      self.sccs = s
    }
  }
  // CHECK-DAG: OF10LocalTypesU_FT_T_L_17SingleClosureEnum
  enum SingleClosureEnum {
    case SCEI(Int)
  }
}

public var singlePattern: Int {
  // CHECK-DAG: VF10LocalTypesg13singlePatternSiL_19SinglePatternStruct
  struct SinglePatternStruct {
    let spsi: Int
  }
  // CHECK-DAG: CF10LocalTypesg13singlePatternSiL_18SinglePatternClass
  class SinglePatternClass {
    let spcs: String
    init(s: String) {
      self.spcs = s
    }
  }
  // CHECK-DAG: OF10LocalTypesg13singlePatternSiL_17SinglePatternEnum
  enum SinglePatternEnum {
    case SPEI(Int)
  }
  return 2
}

public func singleDefaultArgument(i: Int = {
  //CHECK-DAG: VFIF10LocalTypes21singleDefaultArgumentFT1iSi_T_A_U_FT_SiL_27SingleDefaultArgumentStruct
  struct SingleDefaultArgumentStruct {
    let sdasi: Int
  }
  // CHECK-DAG: CFIF10LocalTypes21singleDefaultArgumentFT1iSi_T_A_U_FT_SiL_26SingleDefaultArgumentClass
  class SingleDefaultArgumentClass {
    let sdacs: String
    init(s: String) {
      self.sdacs = s
    }
  }
  // CHECK-DAG: OFIF10LocalTypes21singleDefaultArgumentFT1iSi_T_A_U_FT_SiL_25SingleDefaultArgumentEnum
  enum SingleDefaultArgumentEnum {
    case SDAEI(Int)
  }

  return 2

}()){
  println(i)
}

public func doubleFunc() {
  func innerFunc() {
    // CHECK-DAG: VFF10LocalTypes10doubleFuncFT_T_L_9innerFuncFT_T_L_16DoubleFuncStruct
    struct DoubleFuncStruct {
      let dfsi: Int
    }
    // CHECK-DAG: CFF10LocalTypes10doubleFuncFT_T_L_9innerFuncFT_T_L_15DoubleFuncClass
    class DoubleFuncClass {
      let dfcs: String
      init(s: String) {
        self.dfcs = s
      }
    }
    // CHECK-DAG: OFF10LocalTypes10doubleFuncFT_T_L_9innerFuncFT_T_L_14DoubleFuncEnum
    enum DoubleFuncEnum {
      case DFEI(Int)
    }
  }
  innerFunc()
}

public let doubleClosure: () -> () = {
  let singleClosure: () -> () = {
    // CHECK-DAG: VFF10LocalTypesU0_FT_T_U_FT_T_L_19DoubleClosureStruct
    struct DoubleClosureStruct {
      let dcsi: Int
    }
    // CHECK-DAG: CFF10LocalTypesU0_FT_T_U_FT_T_L_18DoubleClosureClass
    class DoubleClosureClass {
      let dccs: String
      init(s: String) {
        self.dccs = s
      }
    }
    // CHECK-DAG: OFF10LocalTypesU0_FT_T_U_FT_T_L_17DoubleClosureEnum
    enum DoubleClosureEnum {
      case DCEI(Int)
    }
  }
  singleClosure()
}

@transparent
public func transparentFunc() {
  // CHECK-DAG: VF10LocalTypes15transparentFuncFT_T_L_21TransparentFuncStruct
  struct TransparentFuncStruct {
    let tfsi: Int
  }
  // CHECK-DAG: CF10LocalTypes15transparentFuncFT_T_L_20TransparentFuncClass
  class TransparentFuncClass {
    let tfcs: String
    init(s: String) {
      self.tfcs = s
    }
  }
  // CHECK-DAG: OF10LocalTypes15transparentFuncFT_T_L_19TransparentFuncEnum
  enum TransparentFuncEnum {
    case TFEI(Int)
  }
}
