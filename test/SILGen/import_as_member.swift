// RUN: %target-swift-frontend -emit-silgen -I %S/../IDE/Inputs/custom-modules %s 2>&1 | %FileCheck --check-prefix=SIL %s
// REQUIRES: objc_interop
import ImportAsMember.A
import ImportAsMember.Class

public func returnGlobalVar() -> Double {
	return Struct1.globalVar
}
// SIL-LABEL: sil {{.*}}returnGlobalVar{{.*}} () -> Double {
// SIL:   %0 = global_addr @IAMStruct1GlobalVar : $*Double
// SIL:   %2 = load [trivial] %0 : $*Double
// SIL:   return %2 : $Double
// SIL-NEXT: }

// SIL-LABEL: sil {{.*}}anchor{{.*}} () -> () {
func anchor() {}

// SIL-LABEL: sil {{.*}}useClass{{.*}}
// SIL: bb0([[D:%[0-9]+]] : $Double, [[OPTS:%[0-9]+]] : $SomeClass.Options):
public func useClass(d: Double, opts: SomeClass.Options) {
  // SIL: [[CTOR:%[0-9]+]] = function_ref @MakeIAMSomeClass : $@convention(c) (Double) -> @autoreleased SomeClass
  // SIL: [[OBJ:%[0-9]+]] = apply [[CTOR]]([[D]])
  let o = SomeClass(value: d)

  // SIL: [[APPLY_FN:%[0-9]+]] = function_ref @IAMSomeClassApplyOptions : $@convention(c) (SomeClass, SomeClass.Options) -> ()
  // SIL: [[BORROWED_OBJ:%.*]] = begin_borrow [[OBJ]]
  // SIL: apply [[APPLY_FN]]([[BORROWED_OBJ]], [[OPTS]])
  // SIL: end_borrow [[BORROWED_OBJ]] from [[OBJ]]
  // SIL: destroy_value [[OBJ]]
  o.applyOptions(opts)
}

extension SomeClass {
  // SIL-LABEL: sil hidden @_T0So9SomeClassC16import_as_memberEABSd6double_tcfc
  // SIL: bb0([[DOUBLE:%[0-9]+]] : $Double
  // SIL-NOT: value_metatype
  // SIL: [[FNREF:%[0-9]+]] = function_ref @MakeIAMSomeClass
  // SIL: apply [[FNREF]]([[DOUBLE]])
  convenience init(double: Double) {
    self.init(value: double)
  }
}

public func useSpecialInit() -> Struct1 {
  // FIXME: the below triggers an assert, due to number or params mismatch
  // return Struct1(specialLabel:())
}

public func useGlobal() -> Double {
  return Struct1.globalVar
}
