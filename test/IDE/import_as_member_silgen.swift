// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/custom-modules %s 2>&1 | FileCheck --check-prefix=SIL %s

import ImportAsMember

// let iamStruct = Struct1(x: 1.0, y: 1.0, z: 1.0)
// let gVar = Struct1.globalVar

public func returnGlobalVar() -> Int32 {
	return Struct1.globalVar
}
// SIL: sil @_TF24import_as_member_execute15returnGlobalVarFT_Vs5Int32 {{.*}} () -> Int32 {
// SIL-NEXT: bb0:
// SIL-NEXT:   %0 = global_addr @IAMStruct1GlobalVar : $*Int32
// SIL-NEXT:   %1 = metatype $@thin Struct1.Type
// SIL-NEXT:   %2 = load %0 : $*Int32
// SIL-NEXT:   return %2 : $Int32
// SIL-NEXT: }




