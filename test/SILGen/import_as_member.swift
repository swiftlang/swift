// RUN: %target-swift-emit-silgen -I %S/../IDE/Inputs/custom-modules %s | %FileCheck %s
// REQUIRES: objc_interop
import ImportAsMember.A
import ImportAsMember.Class

public func returnGlobalVar() -> Double {
	return Struct1.globalVar
}
// CHECK-LABEL: sil {{.*}}returnGlobalVar{{.*}} () -> Double {
// CHECK:   %0 = global_addr @IAMStruct1GlobalVar : $*Double
// CHECK:   [[READ:%.*]] = begin_access [read] [dynamic] %0 : $*Double
// CHECK:   [[VAL:%.*]] = load [trivial] [[READ]] : $*Double
// CHECK:   return [[VAL]] : $Double
// CHECK-NEXT: }

// N.B. Whether by design or due to a bug, nullable NSString globals
// still import as non-null.
public func returnStringGlobalVar() -> String {
  return Panda.cutenessFactor
}
// CHECK-LABEL: sil {{.*}}returnStringGlobalVar{{.*}} () -> @owned String {
// CHECK:   %0 = global_addr @PKPandaCutenessFactor : $*NSString
// CHECK:   [[VAL:%.*]] = load [copy] %0 : $*NSString
// CHECK:   [[BRIDGE:%.*]] = function_ref @$SSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK:   [[RESULT:%.*]] = apply [[BRIDGE]](
// CHECK:   return [[RESULT]] : $String
// CHECK-NEXT: }

public func returnNullableStringGlobalVar() -> String? {
  return Panda.cuddlynessFactor
}
// CHECK-LABEL: sil {{.*}}returnNullableStringGlobalVar{{.*}} () -> @owned Optional<String> {
// CHECK:   %0 = global_addr @PKPandaCuddlynessFactor : $*NSString
// CHECK:   [[VAL:%.*]] = load [copy] %0 : $*NSString
// CHECK:   [[BRIDGE:%.*]] = function_ref @$SSS10FoundationE36_unconditionallyBridgeFromObjectiveCySSSo8NSStringCSgFZ
// CHECK:   [[RESULT:%.*]] = apply [[BRIDGE]](
// CHECK:   [[SOME:%.*]] = enum $Optional<String>, #Optional.some!enumelt.1, [[RESULT]]
// CHECK:   return [[SOME]] : $Optional<String>
// CHECK-NEXT: }

// CHECK-LABEL: sil {{.*}}useClass{{.*}}
// CHECK: bb0([[D:%[0-9]+]] : $Double, [[OPTS:%[0-9]+]] : $SomeClass.Options):
public func useClass(d: Double, opts: SomeClass.Options) {
  // CHECK: [[CTOR:%[0-9]+]] = function_ref @MakeIAMSomeClass : $@convention(c) (Double) -> @autoreleased SomeClass
  // CHECK: [[OBJ:%[0-9]+]] = apply [[CTOR]]([[D]])
  let o = SomeClass(value: d)

  // CHECK: [[BORROWED_OBJ:%.*]] = begin_borrow [[OBJ]]
  // CHECK: [[APPLY_FN:%[0-9]+]] = function_ref @IAMSomeClassApplyOptions : $@convention(c) (SomeClass, SomeClass.Options) -> ()
  // CHECK: apply [[APPLY_FN]]([[BORROWED_OBJ]], [[OPTS]])
  // CHECK: end_borrow [[BORROWED_OBJ]] from [[OBJ]]
  // CHECK: destroy_value [[OBJ]]
  o.applyOptions(opts)
}

extension SomeClass {
  // CHECK-LABEL: sil hidden @$SSo12IAMSomeClassC16import_as_memberE6doubleABSd_tcfc
  // CHECK: bb0([[DOUBLE:%[0-9]+]] : $Double
  // CHECK-NOT: value_metatype
  // CHECK: [[FNREF:%[0-9]+]] = function_ref @MakeIAMSomeClass
  // CHECK: apply [[FNREF]]([[DOUBLE]])
  convenience init(double: Double) {
    self.init(value: double)
  }
}

public func useSpecialInit() -> Struct1 {
  // FIXME: the below triggers an assert, due to number or params mismatch
  // return Struct1(specialLabel:())
}

