// RUN: %target-swift-frontend %s -O -module-name=test -disable-availability-checking -emit-sil | %FileCheck %s

public struct S {
    var i: Int
    var obj: AnyObject
    var existential: Any
}

public struct S2 {
    var optionalInt: Int?
    var obj: AnyObject?
    var existential: Any?
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

// CHECK-LABEL: sil @$s4test12getIntSwitch4fromSiSgAA1SVSg_tF  :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test12getIntSwitch4fromSiSgAA1SVSg_tF'
public func getIntSwitch(from optionalStruct: S?) -> Int? {
    switch optionalStruct {
        case .some(let myStruct): myStruct.i
        case .none: nil
    }
}

// CHECK-LABEL: sil @$s4test11getIntIfLet4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test11getIntIfLet4fromSiSgAA1SVSg_tF'
public func getIntIfLet(from optionalStruct: S?) -> Int? {
    if let s = optionalStruct {
        s.i
    } else {
        nil
    }
}

// CHECK-LABEL: sil @$s4test14getIntGuardLet4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test14getIntGuardLet4fromSiSgAA1SVSg_tF'
public func getIntGuardLet(from optionalStruct: S?) -> Int? {
    guard let s = optionalStruct else {
        return nil
    }
    return s.i
}

// CHECK-LABEL: sil @$s4test15getIntIfCaseLet4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test15getIntIfCaseLet4fromSiSgAA1SVSg_tF'
public func getIntIfCaseLet(from optionalStruct: S?) -> Int? {
    if case .some(let s) = optionalStruct {
        return s.i
    }
    return nil
}

// CHECK-LABEL: sil @$s4test9getIntMap4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test9getIntMap4fromSiSgAA1SVSg_tF'
public func getIntMap(from optionalStruct: S?) -> Int? {
    optionalStruct.map { $0.i }
}

// CHECK-LABEL: sil @$s4test36getIntConditionallyUnsafelyUnwrapped4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test36getIntConditionallyUnsafelyUnwrapped4fromSiSgAA1SVSg_tF'
public func getIntConditionallyUnsafelyUnwrapped(from optionalStruct: S?) -> Int? {
    optionalStruct == nil ? nil :optionalStruct.unsafelyUnwrapped.i
}

// CHECK-LABEL: sil @$s4test33getIntConditionallyForceUnwrapped4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test33getIntConditionallyForceUnwrapped4fromSiSgAA1SVSg_tF'
public func getIntConditionallyForceUnwrapped(from optionalStruct: S?) -> Int? {
    optionalStruct == nil ? nil :optionalStruct!.i
}

// CHECK-LABEL: sil @$s4test20getIntForceUnwrapped4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test20getIntForceUnwrapped4fromSiSgAA1SVSg_tF'
public func getIntForceUnwrapped(from optionalStruct: S?) -> Int? {
    optionalStruct!.i
}

// CHECK-LABEL: sil @$s4test23getIntUnsafelyUnwrapped4fromSiSgAA1SVSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test23getIntUnsafelyUnwrapped4fromSiSgAA1SVSg_tF'
public func getIntUnsafelyUnwrapped(from optionalStruct: S?) -> Int? {
    optionalStruct.unsafelyUnwrapped.i
}

// CHECK-LABEL: sil @$s4test22getIntOptionalChaining4fromSiSgAA2S2VSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test22getIntOptionalChaining4fromSiSgAA2S2VSg_tF'
public func getIntOptionalChaining(from optionalStruct: S2?) -> Int? {
    optionalStruct?.optionalInt
}

// CHECK-LABEL: sil @$s4test12getIntSwitch4fromSiSgAA2S2VSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test12getIntSwitch4fromSiSgAA2S2VSg_tF'
public func getIntSwitch(from optionalStruct: S2?) -> Int? {
    switch optionalStruct {
        case .some(let myStruct):
        switch myStruct.optionalInt {
        case .some(let i): i
        case .none: nil
        }
        case .none: nil
    }
}

// CHECK-LABEL: sil @$s4test11getIntIfLet4fromSiSgAA2S2VSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test11getIntIfLet4fromSiSgAA2S2VSg_tF'
public func getIntIfLet(from optionalStruct: S2?) -> Int? {
    if let s = optionalStruct, let i = s.optionalInt {
        i
    } else {
        nil
    }
}

// CHECK-LABEL: sil @$s4test14getIntGuardLet4fromSiSgAA2S2VSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test14getIntGuardLet4fromSiSgAA2S2VSg_tF'
public func getIntGuardLet(from optionalStruct: S2?) -> Int? {
    guard let s = optionalStruct, let i = s.optionalInt else {
        return nil
    }
    return i
}

// CHECK-LABEL: sil @$s4test15getIntIfCaseLet4fromSiSgAA2S2VSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test15getIntIfCaseLet4fromSiSgAA2S2VSg_tF'
public func getIntIfCaseLet(from optionalStruct: S2?) -> Int? {
    if case .some(let s) = optionalStruct, case .some(let i) = s.optionalInt {
        return i
    }
    return nil
}

// CHECK-LABEL: sil @$s4test13getIntFlatMap4fromSiSgAA2S2VSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test13getIntFlatMap4fromSiSgAA2S2VSg_tF'
public func getIntFlatMap(from optionalStruct: S2?) -> Int? {
    optionalStruct.flatMap { $0.optionalInt }
}

// CHECK-LABEL: sil @$s4test23getIntUnsafelyUnwrapped4fromSiSgAA2S2VSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test23getIntUnsafelyUnwrapped4fromSiSgAA2S2VSg_tF'
public func getIntUnsafelyUnwrapped(from optionalStruct: S2?) -> Int? {
    optionalStruct.unsafelyUnwrapped.optionalInt.unsafelyUnwrapped
}

// CHECK-LABEL: sil @$s4test20getIntForceUnwrapped4fromSiSgAA2S2VSg_tF :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '$s4test20getIntForceUnwrapped4fromSiSgAA2S2VSg_tF'
public func getIntForceUnwrapped(from optionalStruct: S2?) -> Int? {
    optionalStruct!.optionalInt!
}

func getValue5<Value>(from array: UnsafeBufferPointer<Any>, as: Value.Type) -> Value {
  @_transparent func project<T>( t: T) -> Value {
    unsafeBitCast(t, to: Value.self)
  }
  return _openExistential(array[0], do: project)
}

// CHECK-LABEL: sil {{.*}}@$s4test14getInlineArray4froms0cD0Vy$3_yXlGSRyypG_tF :
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     copy_addr
// CHECK:       } // end sil function '$s4test14getInlineArray4froms0cD0Vy$3_yXlGSRyypG_tF'
public func getInlineArray(from array: UnsafeBufferPointer<Any>) -> InlineArray<4, AnyObject> {
  getValue5(from: array, as: InlineArray<4, AnyObject>.self)
}

