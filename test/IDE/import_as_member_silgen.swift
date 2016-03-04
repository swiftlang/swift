// RUN: %target-swift-frontend -emit-sil -I %S/Inputs/custom-modules %s 2>&1 | FileCheck --check-prefix=SIL %s
import ImportAsMember

public func returnGlobalVar() -> Int32 {
	return Struct1.globalVar
}
// SIL-LABEL: sil {{.*}}returnGlobalVar{{.*}} () -> Int32 {
// SIL:   %0 = global_addr @IAMStruct1GlobalVar : $*Int32
// SIL:   %2 = load %0 : $*Int32
// SIL:   return %2 : $Int32
// SIL-NEXT: }

// SIL-LABEL: sil {{.*}}anchor{{.*}} () -> () {
func anchor() {}

// FIXME: This will now fail until we can SILGen what we can print
// XFAIL: