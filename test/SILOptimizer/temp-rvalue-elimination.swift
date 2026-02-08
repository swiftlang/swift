// RUN: %target-swift-frontend %s -O -module-name=test -emit-sil | %FileCheck %s

public struct S {
    var i: Int
    var obj: AnyObject
    var existential: Any
}

// CHECK-LABEL: sil @$s4test6getInt4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     copy_addr
// CHECK:       } // end sil function '$s4test6getInt4fromSiSgAA1SVSg_tF'
public func getInt(from optionalStruct: S?) -> Int? {
  return optionalStruct?.i
}

// CHECK-LABEL: sil @$s4test6getObj4fromyXlSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     copy_addr
// CHECK:       } // end sil function '$s4test6getObj4fromyXlSgAA1SVSg_tF'
public func getObj(from optionalStruct: S?) -> AnyObject? {
  return optionalStruct?.obj
}

// CHECK-LABEL: sil @$s4test14getExistential4fromypSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test14getExistential4fromypSgAA1SVSg_tF'
public func getExistential(from optionalStruct: S?) -> Any? {
  return optionalStruct?.existential
}

