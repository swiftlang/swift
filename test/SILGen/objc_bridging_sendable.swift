// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types -import-objc-header %S/Inputs/objc_bridging_sendable.h %s | %FileCheck %s

// REQUIRES: objc_interop

// CHECK-LABEL: sil [ossa] @$s22objc_bridging_sendable18passSendableToObjCyys0E0_pF : $@convention(thin) (@in_guaranteed any Sendable) -> () {
// CHECK:  function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @owned AnyObject
// CHECK: objc_method {{%.*}} : $NSBlah, #NSBlah.takeSendable!foreign : (NSBlah) -> ((any Sendable)?) -> (), $@convention(objc_method) (Optional<AnyObject>, NSBlah) -> ()
// CHECK: return
public func passSendableToObjC(_ s: Sendable) {
  NSBlah().takeSendable(s)
}

public func useSendableProperty(_ ns: NSBlah) {
  _ = ns.x
  let _: (Int, Any, String, [Any]) = (42, ns.x, "", [1, 2, 3])
}

// CHECK-LABEL: sil private [ossa] @$s22objc_bridging_sendable23test_use_of_buffer_inityyKFypSo6NSBlahCKXEfU_ : $@convention(thin) @substituted <τ_0_0> (@guaranteed NSBlah) -> (@out τ_0_0, @error any Error) for <Any>
// CHECK: bb0(%0 : $*Any, %1 : @guaranteed $NSBlah):
// CHECK: [[TEST_REF:%.*]] = objc_method %1 : $NSBlah, #NSBlah.test!foreign : (NSBlah) -> () throws -> any Sendable, $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, NSBlah) -> @autoreleased Optional<AnyObject>
// CHECK: [[RESULT:%.*]] = apply [[TEST_REF]]({{.*}}, %1) : $@convention(objc_method) (Optional<AutoreleasingUnsafeMutablePointer<Optional<NSError>>>, NSBlah) -> @autoreleased Optional<AnyObject>
// CHECK: switch_enum [[RESULT]] : $Optional<AnyObject>, case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb2
// CHECK: bb1([[SUCCESS:%.*]] : @owned $AnyObject)
// CHECK-NEXT: [[OPT_RESULT_VALUE:%.*]] = unchecked_ref_cast [[SUCCESS]] : $AnyObject to $Optional<AnyObject>
// CHECK-NEXT: // function_ref _bridgeAnyObjectToAny(_:)
// CHECK-NEXT: [[BRIDGE_INTRINSIC_REF:%.*]] = function_ref @$ss018_bridgeAnyObjectToB0yypyXlSgF : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @out Any
// CHECK-NEXT:  apply [[BRIDGE_INTRINSIC_REF]](%0, [[OPT_RESULT_VALUE]]) : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @out Any
func test_use_of_buffer_init() throws {
  func test<T: Sendable>(_: (NSBlah) throws -> T) rethrows -> T {
    fatalError()
  }

  let _: Any = try test {
      try $0.test()
  }
}

// CHECK-LABEL: sil {{.*}}[ossa] @$s22objc_bridging_sendable5cast1yyXlSgSo6NSBlahCF : $@convention(thin) (@guaranteed NSBlah) -> @owned Optional<AnyObject> {
// CHECK:       bb0(%0 : @guaranteed $NSBlah):
// CHECK:         [[TEST_REF:%.*]] = objc_method %0 : $NSBlah, #NSBlah.sendableNullableNSObject!foreign : (NSBlah) -> () -> (any Sendable)?, $@convention(objc_method) (NSBlah) -> @autoreleased Optional<AnyObject>
// CHECK:         [[RESULT:%.*]] = apply [[TEST_REF]](%0) : $@convention(objc_method) (NSBlah) -> @autoreleased Optional<AnyObject>
// CHECK:         [[OPTIONAL_SENDABLE:%.*]] = alloc_stack $Optional<any Sendable>
// CHECK:         switch_enum [[RESULT]] : $Optional<AnyObject>, case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb2
//
//  AnyObject -> Optional<AnyObject> -> Any -> any Sendable -> Optional<any Sendable>
//
// CHECK:       bb1([[SUCCESS:%.*]] : @owned $AnyObject):
// CHECK:         [[OPT_RESULT_VALUE:%.*]] = unchecked_ref_cast [[SUCCESS]] : $AnyObject to $Optional<AnyObject>
// CHECK:         [[BRIDGE_INTRINSIC_REF:%.*]] = function_ref @$ss018_bridgeAnyObjectToB0yypyXlSgF : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @out Any
// CHECK:         [[INTRINSIC_RET:%.*]] = alloc_stack $Any
// CHECK:         apply [[BRIDGE_INTRINSIC_REF]]([[INTRINSIC_RET]], [[OPT_RESULT_VALUE]]) : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @out Any
// CHECK:         [[ANY_EXT_ADDR:%.*]] = open_existential_addr immutable_access [[INTRINSIC_RET]] : $*Any to $*@opened({{.*}}, Any) Self
// CHECK:         [[ANY_SENDABLE:%.*]] = alloc_stack $any Sendable
// CHECK:         [[ANY_SENDABLE_EXT_ADDR:%.*]] = init_existential_addr [[ANY_SENDABLE]] : $*any Sendable, $@opened({{.*}}, Any) Self
// CHECK:         copy_addr [[ANY_EXT_ADDR]] to [init] [[ANY_SENDABLE_EXT_ADDR]]
// CHECK:         [[OPTIONAL_SENDABLE_DATA_ADDR:%.*]] = init_enum_data_addr [[OPTIONAL_SENDABLE]] : $*Optional<any Sendable>, #Optional.some!enumelt
// CHECK:         copy_addr [take] [[ANY_SENDABLE]] to [init] [[OPTIONAL_SENDABLE_DATA_ADDR]]
// CHECK:         inject_enum_addr [[OPTIONAL_SENDABLE]] : $*Optional<any Sendable>, #Optional.some!enumelt
// CHECK:         br bb3
//
// The main thing we previously had trouble with is:
//   Optional<any Sendable> -> Optional<Any> -> AnyObject
//
// CHECK:       bb3:
// CHECK-NEXT:    [[ABI_COMPATABLE_IMPLICIT_CAST_RESULT:%.*]] = unchecked_addr_cast [[OPTIONAL_SENDABLE]] : $*Optional<any Sendable> to $*Optional<Any>
// CHECK-NEXT:    [[EXPLICIT_CAST_RESULT:%.*]] = alloc_stack $AnyObject
// CHECK:         checked_cast_addr_br take_always Optional<Any> in [[ABI_COMPATABLE_IMPLICIT_CAST_RESULT]] : $*Optional<Any> to AnyObject in [[EXPLICIT_CAST_RESULT]] : $*AnyObject
func cast1(_ b: NSBlah) -> AnyObject? {
  return b.sendableNullableNSObject() as? AnyObject
}

// CHECK-LABEL: sil {{.*}}[ossa] @$s22objc_bridging_sendable5cast2yyXlSgSo6NSBlahCF
// CHECK:         [[OPTIONAL_ANY:%.*]] = alloc_stack $Optional<Any>
// CHECK-NEXT:    switch_enum {{.*}} : $Optional<AnyObject>, case #Optional.some!enumelt: bb1, case #Optional.none!enumelt: bb2
//
// I've abbreviated the CHECK lines for bb1, since `cast1` already covers most of this:
//   AnyObject -> Optional<AnyObject> -> Any -> Optional<Any>
//
// CHECK:         bb1({{.*}} : @owned $AnyObject):
// CHECK:           function_ref @$ss018_bridgeAnyObjectToB0yypyXlSgF : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @out Any
// CHECK:           [[OPTIONAL_ANY_DATA_ADDR:%.*]] = init_enum_data_addr [[OPTIONAL_ANY]] : $*Optional<Any>, #Optional.some!enumelt
// CHECK:           copy_addr [take] {{.*}} to [init] [[OPTIONAL_ANY_DATA_ADDR]] : $*Any
// CHECK:           inject_enum_addr [[OPTIONAL_ANY]] : $*Optional<Any>
//
//   Optional<Any> -> AnyObject
//
// CHECK:       bb3:                                              // Preds: bb2 bb1
// CHECK-NEXT:    [[EXPLICIT_CAST_RESULT:%.*]] = alloc_stack $AnyObject
// CHECK-NEXT:    checked_cast_addr_br take_always Optional<Any> in [[OPTIONAL_ANY]] : $*Optional<Any> to AnyObject in [[EXPLICIT_CAST_RESULT]] : $*AnyObject
func cast2(_ b: NSBlah) -> AnyObject? {
  return b.regularNullableNSObject() as? AnyObject
}

// CHECK-LABEL: sil {{.*}}[ossa] @$s22objc_bridging_sendable5cast3yyXlSgSo6NSBlahCF
// CHECK:         [[BRIDGE_FN:%.*]] = function_ref @$ss018_bridgeAnyObjectToB0yypyXlSgF : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @out Any
// CHECK:         apply [[BRIDGE_FN]]([[ANY:%.*]], {{.*}})
// CHECK:         [[EXPLICIT_CAST_RESULT:%.*]] = alloc_stack $AnyObject
// CHECK:         checked_cast_addr_br take_always Any in [[ANY]] : $*Any to AnyObject in [[EXPLICIT_CAST_RESULT]] : $*AnyObject
func cast3(_ b: NSBlah) -> AnyObject? {
  return b.regularNSObject() as? AnyObject
}