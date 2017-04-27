// Tests lookup and mangling of local types

// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swiftc_driver -v -emit-module -module-name LocalTypes -o %t/LocalTypes.swiftmodule %s
// RUN: %target-swift-ide-test -print-local-types -I %t -module-to-print LocalTypes -source-filename %s | %FileCheck %s

public func singleFunc() {
  // CHECK-DAG: 10LocalTypes10singleFuncyyF06SingleD6StructL_V
  struct SingleFuncStruct {
    let sfsi: Int
  }
  // CHECK-DAG: 10LocalTypes10singleFuncyyF06SingleD5ClassL_C
  class SingleFuncClass {
    let sfcs: String
    init(s: String) {
      self.sfcs = s
    }
  }
  // CHECK-DAG: 10LocalTypes10singleFuncyyF06SingleD4EnumL_O
  enum SingleFuncEnum {
    case SFEI(Int)
  }
}

public func singleFuncWithDuplicates(_ fake: Bool) {
  if fake {
    // CHECK-DAG: 10LocalTypes24singleFuncWithDuplicatesySbF06SingleD6StructL_V
    struct SingleFuncStruct {
      let sfsi: Int
    }
    // CHECK-DAG: 10LocalTypes24singleFuncWithDuplicatesySbF06SingleD5ClassL_C
    class SingleFuncClass {
      let sfcs: String
      init(s: String) {
        self.sfcs = s
      }
    }
    // CHECK-DAG: 10LocalTypes24singleFuncWithDuplicatesySbF06SingleD4EnumL_O
    enum SingleFuncEnum {
      case SFEI(Int)
    }
  } else {
    // CHECK-DAG: 10LocalTypes24singleFuncWithDuplicatesySbF06SingleD6StructL0_V
    struct SingleFuncStruct {
      let sfsi: Int
    }
    // CHECK-DAG: 10LocalTypes24singleFuncWithDuplicatesySbF06SingleD5ClassL0_C
    class SingleFuncClass {
      let sfcs: String
      init(s: String) {
        self.sfcs = s
      }
    }
    // CHECK-DAG: 10LocalTypes24singleFuncWithDuplicatesySbF06SingleD4EnumL0_O
    enum SingleFuncEnum {
      case SFEI(Int)
    }
  }
}

public let singleClosure: () -> () = {
  // CHECK-DAG: 10LocalTypesyycfU_19SingleClosureStructL_V
  struct SingleClosureStruct {
    let scsi: Int
  }
  // CHECK-DAG: 10LocalTypesyycfU_18SingleClosureClassL_C
  class SingleClosureClass {
    let sccs: String
    init(s: String) {
      self.sccs = s
    }
  }
  // CHECK-DAG: 10LocalTypesyycfU_17SingleClosureEnumL_O
  enum SingleClosureEnum {
    case SCEI(Int)
  }
}

public var singlePattern: Int {
  // CHECK-DAG: 10LocalTypes13singlePatternSifg06SingleD6StructL_V
  struct SinglePatternStruct {
    let spsi: Int
  }
  // CHECK-DAG: 10LocalTypes13singlePatternSifg06SingleD5ClassL_C
  class SinglePatternClass {
    let spcs: String
    init(s: String) {
      self.spcs = s
    }
  }
  // CHECK-DAG: 10LocalTypes13singlePatternSifg06SingleD4EnumL_O
  enum SinglePatternEnum {
    case SPEI(Int)
  }
  return 2
}

public func singleDefaultArgument(i: Int = {
  //CHECK-DAG: 10LocalTypes21singleDefaultArgumentySi1i_tFfA_SiycfU_06SingledE6StructL_V
  struct SingleDefaultArgumentStruct {
    let sdasi: Int
  }
  // CHECK-DAG: 10LocalTypes21singleDefaultArgumentySi1i_tFfA_SiycfU_06SingledE5ClassL_C
  class SingleDefaultArgumentClass {
    let sdacs: String
    init(s: String) {
      self.sdacs = s
    }
  }
  // CHECK-DAG: 10LocalTypes21singleDefaultArgumentySi1i_tFfA_SiycfU_06SingledE4EnumL_O
  enum SingleDefaultArgumentEnum {
    case SDAEI(Int)
  }

  return 2

}()){
  print(i)
}

public func doubleFunc() {
  func innerFunc() {
    // CHECK-DAG: 10LocalTypes10doubleFuncyyF05innerD0L_yyF06DoubleD6StructL_V
    struct DoubleFuncStruct {
      let dfsi: Int
    }
    // CHECK-DAG: 10LocalTypes10doubleFuncyyF05innerD0L_yyF06DoubleD5ClassL_C
    class DoubleFuncClass {
      let dfcs: String
      init(s: String) {
        self.dfcs = s
      }
    }
    // CHECK-DAG: 10LocalTypes10doubleFuncyyF05innerD0L_yyF06DoubleD4EnumL_O
    enum DoubleFuncEnum {
      case DFEI(Int)
    }
  }
  innerFunc()
}

public let doubleClosure: () -> () = {
  let singleClosure: () -> () = {
    // CHECK-DAG: 10LocalTypesyycfU0_yycfU_19DoubleClosureStructL_V
    struct DoubleClosureStruct {
      let dcsi: Int
    }
    // CHECK-DAG: 10LocalTypesyycfU0_yycfU_18DoubleClosureClassL_C
    class DoubleClosureClass {
      let dccs: String
      init(s: String) {
        self.dccs = s
      }
    }
    // CHECK-DAG: 10LocalTypesyycfU0_yycfU_17DoubleClosureEnumL_O
    enum DoubleClosureEnum {
      case DCEI(Int)
    }
  }
  singleClosure()
}
