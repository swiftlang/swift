// RUN: %target-swift-emit-silgen -parse-as-library -module-name=test %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-ON
// RUN: %target-swift-emit-silgen -parse-as-library -module-name=test -enforce-exclusivity=none %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-OFF

@exclusivity(checked)
var globalCheckedVar = 1

@exclusivity(unchecked)
var globalUncheckedVar = 1

// CHECK-LABEL: sil [ossa] @$s4test13getCheckedVarSiyF
// CHECK-ON:      begin_access [read] [dynamic]
// CHECK-OFF:     begin_access [read] [dynamic]
// CHECK:       } // end sil function '$s4test13getCheckedVarSiyF'
public func getCheckedVar() -> Int {
  return globalCheckedVar
}

// CHECK-LABEL: sil [ossa] @$s4test15getUncheckedVarSiyF
// CHECK-ON:      begin_access [read] [unsafe]
// CHECK-OFF-NOT: begin_access
// CHECK:       } // end sil function '$s4test15getUncheckedVarSiyF'
public func getUncheckedVar() -> Int {
  return globalUncheckedVar
}

public struct ExclusivityAttrStruct {

// CHECK-LABEL: sil {{.*}}@$s4test21ExclusivityAttrStructV9staticVarSivsZ
// CHECK-ON:      begin_access [modify] [unsafe]
// CHECK:       } // end sil function '$s4test21ExclusivityAttrStructV9staticVarSivsZ'
  @exclusivity(unchecked)
  public static var staticVar: Int = 27
}

public class ExclusivityAttrClass {
// CHECK-LABEL: sil {{.*}}@$s4test20ExclusivityAttrClassC11instanceVarSivs
// CHECK-ON:      begin_access [modify] [unsafe]
// CHECK:       } // end sil function '$s4test20ExclusivityAttrClassC11instanceVarSivs'
  @exclusivity(unchecked)
  public var instanceVar: Int = 27

// CHECK-LABEL: sil {{.*}}@$s4test20ExclusivityAttrClassC18checkedInstanceVarSivs
// CHECK-ON:      begin_access [modify] [dynamic]
// CHECK-OFF:     begin_access [modify] [dynamic]
// CHECK:       } // end sil function '$s4test20ExclusivityAttrClassC18checkedInstanceVarSivs'
  @exclusivity(checked)
  public var checkedInstanceVar: Int = 27

// CHECK-LABEL: sil {{.*}}@$s4test20ExclusivityAttrClassC9staticVarSivsZ
// CHECK-ON:      begin_access [modify] [unsafe]
// CHECK:       } // end sil function '$s4test20ExclusivityAttrClassC9staticVarSivsZ'
  @exclusivity(unchecked)
  public static var staticVar: Int = 27
}

