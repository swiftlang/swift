// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -disable-objc-attr-requires-foundation-module -enable-objc-interop %s -module-name failable_initializers | %FileCheck %s

// High-level tests that silgen properly emits code for failable and throwing
// initializers.

////
// Structs with failable initializers
////

protocol Pachyderm {
  init()
}

class Canary : Pachyderm {
  required init() {}
}

// <rdar://problem/20941576> SILGen crash: Failable struct init cannot delegate to another failable initializer
struct TrivialFailableStruct {
  init?(blah: ()) { }
  init?(wibble: ()) {
    self.init(blah: wibble)
  }
}

struct FailableStruct {
  let x, y: Canary

  init(noFail: ()) {
    x = Canary()
    y = Canary()
  }

  init?(failBeforeInitialization: ()) {
    return nil
  }

  init?(failAfterPartialInitialization: ()) {
    x = Canary()
    return nil
  }

  init?(failAfterFullInitialization: ()) {
    x = Canary()
    y = Canary()
    return nil
  }

  init?(failAfterWholeObjectInitializationByAssignment: ()) {
    self = FailableStruct(noFail: ())
    return nil
  }

  init?(failAfterWholeObjectInitializationByDelegation: ()) {
    self.init(noFail: ())
    return nil
  }

  // Optional to optional
  init?(failDuringDelegation: ()) {
    self.init(failBeforeInitialization: ())
  }

  // IUO to optional
  init!(failDuringDelegation2: ()) {
    self.init(failBeforeInitialization: ())! // unnecessary-but-correct '!'
  }

  // IUO to IUO
  init!(failDuringDelegation3: ()) {
    self.init(failDuringDelegation2: ())! // unnecessary-but-correct '!'
  }

  // non-optional to optional
  init(failDuringDelegation4: ()) {
    self.init(failBeforeInitialization: ())! // necessary '!'
  }

  // non-optional to IUO
  init(failDuringDelegation5: ()) {
    self.init(failDuringDelegation2: ())! // unnecessary-but-correct '!'
  }
}

extension FailableStruct {
  init?(failInExtension: ()) {
    self.init(failInExtension: failInExtension)
  }

  init?(assignInExtension: ()) {
    self = FailableStruct(noFail: ())
  }
}

struct FailableAddrOnlyStruct<T : Pachyderm> {
  var x, y: T

  init(noFail: ()) {
    x = T()
    y = T()
  }

  init?(failBeforeInitialization: ()) {
    return nil
  }

  init?(failAfterPartialInitialization: ()) {
    x = T()
    return nil
  }

  init?(failAfterFullInitialization: ()) {
    x = T()
    y = T()
    return nil
  }

  init?(failAfterWholeObjectInitializationByAssignment: ()) {
    self = FailableAddrOnlyStruct(noFail: ())
    return nil
  }

  init?(failAfterWholeObjectInitializationByDelegation: ()) {
    self.init(noFail: ())
    return nil
  }

  // Optional to optional
  init?(failDuringDelegation: ()) {
    self.init(failBeforeInitialization: ())
  }

  // IUO to optional
  init!(failDuringDelegation2: ()) {
    self.init(failBeforeInitialization: ())! // unnecessary-but-correct '!'
  }

  // non-optional to optional
  init(failDuringDelegation3: ()) {
    self.init(failBeforeInitialization: ())! // necessary '!'
  }

  // non-optional to IUO
  init(failDuringDelegation4: ()) {
    self.init(failDuringDelegation2: ())! // unnecessary-but-correct '!'
  }
}

extension FailableAddrOnlyStruct {
  init?(failInExtension: ()) {
    self.init(failBeforeInitialization: failInExtension)
  }

  init?(assignInExtension: ()) {
    self = FailableAddrOnlyStruct(noFail: ())
  }
}

////
// Structs with throwing initializers
////

func unwrap(_ x: Int) throws -> Int { return x }

struct ThrowStruct {
  var x: Canary

  init(throws: ()) throws { x = Canary() }

  init(noThrows: ()) { x = Canary() }

  init(throwsBeforeDelegation: Int) throws {
    try unwrap(throwsBeforeDelegation)
    self.init(noThrows: ())
  }

  init(throwsBeforeOrDuringDelegation: Int) throws {
    try unwrap(throwsBeforeOrDuringDelegation)
    try self.init(throws: ())
  }

  init(throwsBeforeOrDuringDelegation2: Int) throws {
    try self.init(throwsBeforeDelegation: unwrap(throwsBeforeOrDuringDelegation2))
  }

  init(throwsDuringDelegation: Int) throws {
    try self.init(throws: ())
  }

  init(throwsAfterDelegation: Int) throws {
    self.init(noThrows: ())
    try unwrap(throwsAfterDelegation)
  }

  init(throwsDuringOrAfterDelegation: Int) throws {
    try self.init(throws: ())
    try unwrap(throwsDuringOrAfterDelegation)
  }

  init(throwsBeforeOrAfterDelegation: Int) throws {
    try unwrap(throwsBeforeOrAfterDelegation)
    self.init(noThrows: ())
    try unwrap(throwsBeforeOrAfterDelegation)
  }

  init(throwsBeforeSelfReplacement: Int) throws {
    try unwrap(throwsBeforeSelfReplacement)
    self = ThrowStruct(noThrows: ())
  }

  init(throwsDuringSelfReplacement: Int) throws {
    try self = ThrowStruct(throws: ())
  }

  init(throwsAfterSelfReplacement: Int) throws {
    self = ThrowStruct(noThrows: ())
    try unwrap(throwsAfterSelfReplacement)
  }

  init(nonFailable: ()) {
    try! self.init(throws: ())
  }

  init(nonFailable2: ()) throws {
    self.init(failable: ())!
  }

  init?(failableAndThrows: ()) throws {
    self.init(noThrows: ())
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers11ThrowStructV0A0ACSgyt_tcfC
  // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin ThrowStruct.Type):
  // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$s21failable_initializers11ThrowStructV6throwsACyt_tKcfC
  // CHECK-NEXT: try_apply [[DELEG_INIT]]([[SELF_META]]) : $@convention(method) (@thin ThrowStruct.Type) -> (@owned ThrowStruct, @error any Error), normal [[SUCC_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB]]([[RESULT:%[0-9]+]] : @owned $ThrowStruct):
  // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.some!enumelt, [[RESULT]]
  // CHECK-NEXT: br bb2([[INJECT_INTO_OPT]] : $Optional<ThrowStruct>)
  //
  // CHECK: bb2([[OPT_RESULT:%[0-9]+]] : @owned $Optional<ThrowStruct>):
  // CHECK: [[SELECT:%[0-9]+]] = select_enum [[OPT_RESULT]]
  // CHECK-NEXT: cond_br [[SELECT]], [[SOME_BB:bb[0-9]+]], [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[NONE_BB]]:
  // CHECK-NEXT: destroy_value [[OPT_RESULT]]
  // CHECK-NEXT: br bb5
  //
  // CHECK: [[SOME_BB]]:
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = unchecked_enum_data [[OPT_RESULT]] : {{.*}}, #Optional.some!enumelt
  // CHECK-NEXT: assign [[RESULT]] to [[DEST:%[0-9]+]]
  // CHECK-NEXT: [[RESULT_CP:%[0-9]+]] = load [copy] [[DEST]]
  // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.some!enumelt, [[RESULT_CP]]
  // CHECK: br bb6([[INJECT_INTO_OPT]] : $Optional<ThrowStruct>)
  //
  // CHECK: bb5:
  // CHECK: [[NIL:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.none!enumelt
  // CHECK-NEXT: br bb6([[NIL]] : $Optional<ThrowStruct>)
  //
  // CHECK: bb6([[RET:%[0-9]+]] : @owned $Optional<ThrowStruct>):
  // CHECK-NEXT: return [[RET]]
  //
  // CHECK: bb7([[ERR:%[0-9]+]] : @owned $any Error):
  // CHECK-NEXT: destroy_value [[ERR]]
  // CHECK-NEXT: [[NIL:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.none!enumelt
  // CHECK-NEXT: br bb2([[NIL]] : $Optional<ThrowStruct>)
  //
  // CHECK: [[ERROR_BB]]([[ERR:%[0-9]+]] : @owned $any Error):
  // CHECK-NEXT: br bb7([[ERR]] : $any Error)
  // CHECK-NEXT: }
  init?(failable: ()) {
    try? self.init(throws: ())
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers11ThrowStructV9failable2ACSgyt_tcfC
  // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin ThrowStruct.Type):
  // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$s21failable_initializers11ThrowStructV0A9AndThrowsACSgyt_tKcfC
  // CHECK-NEXT: try_apply [[DELEG_INIT]]([[SELF_META]]) : $@convention(method) (@thin ThrowStruct.Type) -> (@owned Optional<ThrowStruct>, @error any Error), normal [[SUCC_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB]]([[OPT_RESULT:%[0-9]+]] : @owned $Optional<ThrowStruct>):
  // CHECK: [[SELECT:%[0-9]+]] = select_enum [[OPT_RESULT]]
  // CHECK-NEXT: cond_br [[SELECT]], [[SOME_BB:bb[0-9]+]], [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[NONE_BB]]:
  // CHECK-NEXT: destroy_value [[OPT_RESULT]]
  // CHECK-NEXT: br bb4
  //
  // CHECK: [[SOME_BB]]:
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = unchecked_enum_data [[OPT_RESULT]] : $Optional<ThrowStruct>, #Optional.some!enumelt
  // CHECK-NEXT: assign [[RESULT]] to [[DEST:%[0-9]+]]
  // CHECK-NEXT: [[RESULT_CP:%[0-9]+]] = load [copy] [[DEST]]
  // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.some!enumelt, [[RESULT_CP]]
  // CHECK: br bb5([[INJECT_INTO_OPT]] : $Optional<ThrowStruct>)
  //
  // CHECK: bb4:
  // CHECK: [[NIL:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.none!enumelt
  // CHECK-NEXT: br bb5([[NIL]] : $Optional<ThrowStruct>)
  //
  // CHECK: bb5([[RET:%[0-9]+]] : @owned $Optional<ThrowStruct>):
  // CHECK-NEXT: return [[RET]]
  //
  // CHECK: bb6([[ERR:%[0-9]+]] : @owned $any Error):
  // CHECK: unreachable
  //
  // CHECK: [[ERROR_BB]]([[ERR:%[0-9]+]] : @owned $any Error):
  // CHECK-NEXT: br bb6([[ERR]] : $any Error)
  // CHECK-NEXT: }
  init?(failable2: ()) {
    try! self.init(failableAndThrows: ())
  }

  init?(failable3: ()) {
    try? self.init(failableAndThrows: ())!
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers11ThrowStructV9failable4ACSgyt_tcfC
  // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thin ThrowStruct.Type):
  // CHECK: [[DELEG_INIT:%[0-9]+]] = function_ref @$s21failable_initializers11ThrowStructV0A9AndThrowsACSgyt_tKcfC
  // CHECK-NEXT: try_apply [[DELEG_INIT]]([[SELF_META]]) : $@convention(method) (@thin ThrowStruct.Type) -> (@owned Optional<ThrowStruct>, @error any Error), normal [[SUCC_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB]]([[OPT_RESULT:%[0-9]+]] : @owned $Optional<ThrowStruct>):
  // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum {{.*}}, #Optional.some!enumelt, [[OPT_RESULT]]
  // CHECK-NEXT: br bb2([[INJECT_INTO_OPT]] : $Optional<Optional<ThrowStruct>>)
  //
  // CHECK: bb2([[OPT_OPT_RESULT:%[0-9]+]] : @owned $Optional<Optional<ThrowStruct>>):
  // CHECK-NEXT: switch_enum [[OPT_OPT_RESULT]] : {{.*}}, case #Optional.some!enumelt: [[OPT_OPT_SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[OPT_OPT_NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[OPT_OPT_SOME_BB]]([[OPT_RESULT:%[0-9]+]] : @owned $Optional<ThrowStruct>):
  // CHECK-NEXT: br bb5([[OPT_RESULT]] : $Optional<ThrowStruct>)
  //
  // CHECK: [[OPT_OPT_NONE_BB]]:
  // CHECK-NEXT: [[NIL:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.none!enumelt
  // CHECK-NEXT: br bb5([[NIL]] : $Optional<ThrowStruct>)
  //
  // CHECK: bb5([[OPT_RESULT:%[0-9]+]] : @owned $Optional<ThrowStruct>):
  // CHECK: [[SELECT:%[0-9]+]] = select_enum [[OPT_RESULT]]
  // CHECK-NEXT: cond_br [[SELECT]], [[OPT_SOME_BB:bb[0-9]+]], [[OPT_NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[OPT_NONE_BB]]:
  // CHECK-NEXT: destroy_value [[OPT_RESULT]]
  // CHECK-NEXT: br bb8
  //
  // CHECK: [[OPT_SOME_BB]]:
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = unchecked_enum_data [[OPT_RESULT]] : {{.*}}, #Optional.some!enumelt
  // CHECK-NEXT: assign [[RESULT]] to [[DEST:%[0-9]+]]
  // CHECK-NEXT: [[RESULT_CP:%[0-9]+]] = load [copy] [[DEST]]
  // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.some!enumelt, [[RESULT_CP]]
  // CHECK: br bb9([[INJECT_INTO_OPT]] : $Optional<ThrowStruct>)
  //
  // CHECK: bb8:
  // CHECK: [[NIL:%[0-9]+]] = enum $Optional<ThrowStruct>, #Optional.none!enumelt
  // CHECK-NEXT: br bb9([[NIL]] : $Optional<ThrowStruct>)
  //
  // CHECK: bb9([[RET:%[0-9]+]] : @owned $Optional<ThrowStruct>):
  // CHECK-NEXT: return [[RET]]
  //
  // CHECK: bb10([[ERR:%[0-9]+]] : @owned $any Error):
  // CHECK-NEXT: destroy_value [[ERR]]
  // CHECK-NEXT: [[NIL:%[0-9]+]] = enum $Optional<Optional<ThrowStruct>>, #Optional.none!enumelt
  // CHECK-NEXT: br bb2([[NIL]] : $Optional<Optional<ThrowStruct>>)
  //
  // CHECK: [[ERROR_BB]]([[ERR:%[0-9]+]] : @owned $any Error):
  // CHECK-NEXT: br bb10([[ERR]] : $any Error)
  // CHECK-NEXT: }
  init?(failable4: ()) {
    try? self.init(failableAndThrows: ())
  }
}

extension ThrowStruct {
  init(throwsInExtension: ()) throws {
    try self.init(throws: throwsInExtension)
  }

  init(assignInExtension: ()) throws {
    try self = ThrowStruct(throws: ())
  }
}

struct ThrowAddrOnlyStruct<T : Pachyderm> {
  var x : T

  init(fail: ()) throws { x = T() }

  init(noFail: ()) { x = T() }

  init(failBeforeDelegation: Int) throws {
    try unwrap(failBeforeDelegation)
    self.init(noFail: ())
  }

  init(failBeforeOrDuringDelegation: Int) throws {
    try unwrap(failBeforeOrDuringDelegation)
    try self.init(fail: ())
  }

  init(failBeforeOrDuringDelegation2: Int) throws {
    try self.init(failBeforeDelegation: unwrap(failBeforeOrDuringDelegation2))
  }

  init(failDuringDelegation: Int) throws {
    try self.init(fail: ())
  }

  init(failAfterDelegation: Int) throws {
    self.init(noFail: ())
    try unwrap(failAfterDelegation)
  }

  init(failDuringOrAfterDelegation: Int) throws {
    try self.init(fail: ())
    try unwrap(failDuringOrAfterDelegation)
  }

  init(failBeforeOrAfterDelegation: Int) throws {
    try unwrap(failBeforeOrAfterDelegation)
    self.init(noFail: ())
    try unwrap(failBeforeOrAfterDelegation)
  }

  init?(throwsToOptional: Int) {
    try? self.init(failDuringDelegation: throwsToOptional)
  }

  init(throwsToIUO: Int) {
    try! self.init(failDuringDelegation: throwsToIUO)
  }

  init?(throwsToOptionalThrows: Int) throws {
    try? self.init(fail: ())
  }

  init(throwsOptionalToThrows: Int) throws {
    self.init(throwsToOptional: throwsOptionalToThrows)!
  }

  init?(throwsOptionalToOptional: Int) {
    try! self.init(throwsOptionalToThrows: throwsOptionalToOptional)
  }

  init(failBeforeSelfReplacement: Int) throws {
    try unwrap(failBeforeSelfReplacement)
    self = ThrowAddrOnlyStruct(noFail: ())
  }

  init(failAfterSelfReplacement: Int) throws {
    self = ThrowAddrOnlyStruct(noFail: ())
    try unwrap(failAfterSelfReplacement)
  }
}

extension ThrowAddrOnlyStruct {
  init(failInExtension: ()) throws {
    try self.init(fail: failInExtension)
  }

  init(assignInExtension: ()) throws {
    self = ThrowAddrOnlyStruct(noFail: ())
  }
}

////
// Classes
////

////
// Classes with failable initializers
////

class FailableBaseClass {
  var member: Canary

  init(noFail: ()) {
    member = Canary()
  }

  init?(failBeforeFullInitialization: ()) {
    return nil
  }

  init?(failAfterFullInitialization: ()) {
    member = Canary()
    return nil
  }

  convenience init?(failBeforeDelegation: ()) {
    return nil
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17FailableBaseClassC19failAfterDelegationACSgyt_tcfC
  // CHECK: bb0([[SELF_META:%.*]] : $@thick FailableBaseClass.Type):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[SELF_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_LIFETIME]]
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[SELF_META]])
  // CHECK:   destroy_value [[MARKED_SELF_BOX]]
  // CHECK:   [[RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.none!enumelt
  // CHECK:   br bb2([[RESULT]] : $Optional<FailableBaseClass>)
  // CHECK: bb2([[RESULT:%.*]] : @owned $Optional<FailableBaseClass>):
  // CHECK:   return [[RESULT]]
  // CHECK-NEXT: }
  convenience init?(failAfterDelegation: ()) {
    self.init(noFail: ())
    return nil
  }

  // Optional to optional
  //
  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17FailableBaseClassC20failDuringDelegationACSgyt_tcfC
  // CHECK: bb0([[SELF_META:%.*]] : $@thick FailableBaseClass.Type):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[SELF_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_LIFETIME]]
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[SELF_META]])
  // CHECK:   cond_br {{.*}}, [[SUCC_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[FAIL_BB:bb[0-9]+]]:
  // CHECK:   destroy_value [[NEW_SELF]]
  // CHECK:   br [[FAIL_EPILOG_BB:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB]]:
  // CHECK:   [[UNWRAPPED_NEW_SELF:%.*]] = unchecked_enum_data [[NEW_SELF]] : $Optional<FailableBaseClass>, #Optional.some!enumelt
  // CHECK:   assign [[UNWRAPPED_NEW_SELF]] to [[PB_BOX]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PB_BOX]]
  // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.some!enumelt, [[RESULT]]
  // CHECK:   destroy_value [[MARKED_SELF_BOX]]
  // CHECK:   br [[EPILOG_BB:bb[0-9]+]]([[WRAPPED_RESULT]]
  //
  // CHECK: [[FAIL_EPILOG_BB]]:
  // CHECK:   destroy_value [[MARKED_SELF_BOX]]
  // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.none!enumelt
  // CHECK:   br [[EPILOG_BB]]([[WRAPPED_RESULT]]
  //
  // CHECK: [[EPILOG_BB]]([[WRAPPED_RESULT:%.*]] : @owned $Optional<FailableBaseClass>):
  // CHECK:   return [[WRAPPED_RESULT]]
  convenience init?(failDuringDelegation: ()) {
    self.init(failBeforeFullInitialization: ())
  }

  // IUO to optional
  //
  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17FailableBaseClassC21failDuringDelegation2ACSgyt_tcfC
  // CHECK: bb0([[SELF_META:%.*]] : $@thick FailableBaseClass.Type):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[SELF_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_LIFETIME]]
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[SELF_META]])
  // CHECK:   switch_enum [[NEW_SELF]] : $Optional<FailableBaseClass>, case #Optional.some!enumelt: [[SUCC_BB:bb[0-9]+]], case #Optional.none!enumelt: [[FAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[FAIL_BB]]:
  // CHECK:   unreachable
  //
  // CHECK: [[SUCC_BB]]([[RESULT:%.*]] : @owned $FailableBaseClass):
  // CHECK:   assign [[RESULT]] to [[PB_BOX]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PB_BOX]]
  // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.some!enumelt, [[RESULT]] : $FailableBaseClass
  // CHECK:   destroy_value [[MARKED_SELF_BOX]]
  // CHECK:   br [[EPILOG_BB:bb[0-9]+]]([[WRAPPED_RESULT]]
  //
  // CHECK: [[EPILOG_BB]]([[WRAPPED_RESULT:%.*]] : @owned $Optional<FailableBaseClass>):
  // CHECK:   return [[WRAPPED_RESULT]]
  // CHECK-NEXT: }
  convenience init!(failDuringDelegation2: ()) {
    self.init(failBeforeFullInitialization: ())! // unnecessary-but-correct '!'
  }

  // Optional to non-optional
  //
  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17FailableBaseClassC21failDuringDelegation3ACSgyt_tcfC
  // CHECK: bb0([[SELF_META:%[0-9]+]] : $@thick FailableBaseClass.Type):
  // CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK-NEXT: [[MARKED_SELF_BOX:%[0-9]+]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK-NEXT: [[SELF_LIFETIME:%[0-9]+]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
  // CHECK-NEXT: [[PB_BOX:%[0-9]+]] = project_box [[SELF_LIFETIME]]
  // CHECK: [[DELEG_INIT:%[0-9]+]] = class_method [[SELF_META]] : $@thick FailableBaseClass.Type, #FailableBaseClass.init!allocator
  // CHECK-NEXT: [[RESULT:%[0-9]+]] = apply [[DELEG_INIT]]([[SELF_META]])
  // CHECK-NEXT: assign [[RESULT]] to [[PB_BOX]]
  // CHECK-NEXT: [[RESULT_COPY:%[0-9]+]] = load [copy] [[PB_BOX]]
  // CHECK-NEXT: [[INJECT_INTO_OPT:%[0-9]+]] = enum $Optional<FailableBaseClass>, #Optional.some!enumelt, [[RESULT_COPY]]
  // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: br bb2([[INJECT_INTO_OPT]] : $Optional<FailableBaseClass>)
  //
  // FIXME: Dead block
  // CHECK: bb1:
  //
  // CHECK: bb2([[ARG:%[0-9]+]] : @owned $Optional<FailableBaseClass>):
  // CHECK-NEXT: return [[ARG]]
  // CHECK-NEXT: }
  convenience init?(failDuringDelegation3: ()) {
    self.init(noFail: ())
  }

  // IUO to IUO
  convenience init!(noFailDuringDelegation: ()) {
    self.init(failDuringDelegation2: ())! // unnecessary-but-correct '!'
  }

  // non-optional to optional
  convenience init(noFailDuringDelegation2: ()) {
    self.init(failBeforeFullInitialization: ())! // necessary '!'
  }
}

extension FailableBaseClass {
  convenience init?(failInExtension: ()) throws {
    self.init(failBeforeFullInitialization: failInExtension)
  }
}

// Chaining to failable initializers in a superclass
class FailableDerivedClass : FailableBaseClass {
  var otherMember: Canary

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers20FailableDerivedClassC27derivedFailBeforeDelegationACSgyt_tcfc : $@convention(method) (@owned FailableDerivedClass) -> @owned Optional<FailableDerivedClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : @owned $FailableDerivedClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableDerivedClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [derivedself] [[SELF_BOX]]
  // CHECK:   [[SELF_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_LIFETIME]]
  // CHECK:   store [[OLD_SELF]] to [init] [[PB_BOX]]
  // CHECK-NEXT: br bb1
  //
  // CHECK: bb1:
  // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: [[RESULT:%.*]] = enum $Optional<FailableDerivedClass>, #Optional.none!enumelt
  // CHECK-NEXT: br bb2([[RESULT]]
  //
  // CHECK: bb2([[RESULT:%.*]] : @owned $Optional<FailableDerivedClass>):
  // CHECK-NEXT: return [[RESULT]]
  init?(derivedFailBeforeDelegation: ()) {
    return nil
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers20FailableDerivedClassC27derivedFailDuringDelegationACSgyt_tcfc : $@convention(method) (@owned FailableDerivedClass) -> @owned Optional<FailableDerivedClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : @owned $FailableDerivedClass):
  init?(derivedFailDuringDelegation: ()) {
    // First initialize the lvalue for self.
    // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableDerivedClass }, let, name "self"
    // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [derivedself] [[SELF_BOX]]
    // CHECK:   [[SELF_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
    // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_LIFETIME]]
    // CHECK:   store [[OLD_SELF]] to [init] [[PB_BOX]]
    //
    // Then assign canary to other member using a borrow.
    // CHECK:   [[BORROWED_SELF:%.*]] = load_borrow [[PB_BOX]]
    // CHECK:   [[CANARY_VALUE:%.*]] = apply
    // CHECK:   [[CANARY_GEP:%.*]] = ref_element_addr [[BORROWED_SELF]]
    // CHECK:   [[WRITE:%.*]] = begin_access [modify] [dynamic] [[CANARY_GEP]] : $*Canary
    // CHECK:   assign [[CANARY_VALUE]] to [[WRITE]]
    self.otherMember = Canary()

    // Finally, begin the super init sequence.
    // CHECK:   [[LOADED_SELF:%.*]] = load [take] [[PB_BOX]]
    // CHECK:   [[UPCAST_SELF:%.*]] = upcast [[LOADED_SELF]]
    // CHECK:   [[OPT_NEW_SELF:%.*]] = apply {{.*}}([[UPCAST_SELF]]) : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass>
    // CHECK:   [[IS_NIL:%.*]] = select_enum [[OPT_NEW_SELF]]
    // CHECK:   cond_br [[IS_NIL]], [[SUCC_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]
    //
    // And then return nil making sure that we do not insert any extra values anywhere.
    // CHECK: [[SUCC_BB]]:
    // CHECK:   [[NEW_SELF:%.*]] = unchecked_enum_data [[OPT_NEW_SELF]]
    // CHECK:   [[DOWNCAST_NEW_SELF:%.*]] = unchecked_ref_cast [[NEW_SELF]]
    // CHECK:   store [[DOWNCAST_NEW_SELF]] to [init] [[PB_BOX]]
    // CHECK:   [[RESULT:%.*]] = load [copy] [[PB_BOX]]
    // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableDerivedClass>, #Optional.some!enumelt, [[RESULT]]
    // CHECK:   destroy_value [[MARKED_SELF_BOX]]
    // CHECK:   br [[EPILOG_BB:bb[0-9]+]]([[WRAPPED_RESULT]]
    //
    // CHECK: [[FAIL_BB]]:
    // CHECK:   destroy_value [[MARKED_SELF_BOX]]
    super.init(failBeforeFullInitialization: ())
  }

  init?(derivedFailAfterDelegation: ()) {
    self.otherMember = Canary()
    super.init(noFail: ())
    return nil
  }

  // non-optional to IUO
  init(derivedNoFailDuringDelegation: ()) {
    self.otherMember = Canary()
    super.init(failAfterFullInitialization: ())! // necessary '!'
  }

  // IUO to IUO
  init!(derivedFailDuringDelegation2: ()) {
    self.otherMember = Canary()
    super.init(failAfterFullInitialization: ())! // unnecessary-but-correct '!'
  }
}

extension FailableDerivedClass {
  convenience init?(derivedFailInExtension: ()) throws {
    self.init(derivedFailDuringDelegation: derivedFailInExtension)
  }
}

////
// Classes with throwing initializers
////

class ThrowBaseClass {
  required init() throws {}
  required init(throwingCanary: Canary) throws {}
  init(canary: Canary) {}
  init(noFail: ()) {}
  init(fail: Int) throws {}
  init(noFail: Int) {}
}

class ThrowDerivedClass : ThrowBaseClass {
  var canary: Canary?

  required init(throwingCanary: Canary) throws {
  }

  required init() throws {
    try super.init()
  }

  override init(noFail: ()) {
    try! super.init()
  }

  override init(fail: Int) throws {}
  override init(noFail: Int) {}

  // ---- Delegating to super

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17ThrowDerivedClassC30delegatingFailBeforeDelegationACSi_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error any Error) {
  // CHECK: bb0(
  // First initialize.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARK_UNINIT]]
  // CHECK:   [[PROJ:%.*]] = project_box [[LIFETIME]]
  // CHECK:   store {{%.*}} to [init] [[PROJ]]
  //
  // Then initialize the canary with nil. We are able to borrow the initialized self to avoid retain/release overhead.
  // CHECK:   [[SELF:%.*]] = load_borrow [[PROJ]]
  // CHECK:   [[CANARY_ADDR:%.*]] = ref_element_addr [[SELF]]
  // CHECK:   [[CANARY_FUNC:%.*]] = function_ref @$s21failable_initializers17ThrowDerivedClassC6canaryAA6CanaryCSgvpfi :
  // CHECK:   [[OPT_CANARY:%.*]] = apply [[CANARY_FUNC]]()
  // CHECK:   store [[OPT_CANARY]] to [init] [[CANARY_ADDR]]
  // CHECK:   end_borrow [[SELF]]
  //
  // Now we perform the unwrap.
  // CHECK:   [[UNWRAP_FN:%.*]] = function_ref @$s21failable_initializers6unwrapyS2iKF : $@convention(thin)
  // CHECK:   try_apply [[UNWRAP_FN]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // CHECK: [[NORMAL_BB]](
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[SELF_BASE:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[BASE_INIT_FN:%.*]] = function_ref @$s21failable_initializers14ThrowBaseClassC6noFailACyt_tcfc : $@convention(method)
  // CHECK:   [[SELF_INIT_BASE:%.*]] = apply [[BASE_INIT_FN]]([[SELF_BASE]])
  // CHECK:   [[SELF:%.*]] = unchecked_ref_cast [[SELF_INIT_BASE]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[SELF]] to [init] [[PROJ]]
  // CHECK:   [[SELF:%.*]] = load [copy] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   return [[SELF]]
  //
  // Finally the error BB. We do not touch self since self is still in the
  // box implying that destroying MARK_UNINIT will destroy it for us.
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   throw [[ERROR]]
  // CHECK: } // end sil function '$s21failable_initializers17ThrowDerivedClassC30delegatingFailBeforeDelegationACSi_tKcfc'
  init(delegatingFailBeforeDelegation : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    super.init(noFail: ())
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17ThrowDerivedClassC41delegatingFailDuringDelegationArgEmissionACSi_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error any Error) {
  // CHECK: bb0(
  // First initialize.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARK_UNINIT]]
  // CHECK:   [[PROJ:%.*]] = project_box [[LIFETIME]]
  // CHECK:   store {{%.*}} to [init] [[PROJ]]
  //
  // Then initialize the canary with nil. We are able to borrow the initialized self to avoid retain/release overhead.
  // CHECK:   [[SELF:%.*]] = load_borrow [[PROJ]]
  // CHECK:   [[CANARY_ADDR:%.*]] = ref_element_addr [[SELF]]
  // CHECK:   [[CANARY_FUNC:%.*]] = function_ref @$s21failable_initializers17ThrowDerivedClassC6canaryAA6CanaryCSgvpfi :
  // CHECK:   [[OPT_CANARY:%.*]] = apply [[CANARY_FUNC]]()
  // CHECK:   store [[OPT_CANARY]] to [init] [[CANARY_ADDR]]
  // CHECK:   end_borrow [[SELF]]
  //
  // Now we begin argument emission where we perform the unwrap.
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[BASE_SELF:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[UNWRAP_FN:%.*]] = function_ref @$s21failable_initializers6unwrapyS2iKF : $@convention(thin)
  // CHECK:   try_apply [[UNWRAP_FN]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // Now we emit the call to the initializer. Notice how we return self back to
  // its memory location before any other work is done.
  // CHECK: [[NORMAL_BB]](
  // CHECK:   [[INIT_FN:%.*]] = function_ref @$s21failable_initializers14ThrowBaseClassC6noFailACSi_tcfc : $@convention(method)
  // CHECK:   [[BASE_SELF_INIT:%.*]] = apply [[INIT_FN]]({{%.*}}, [[BASE_SELF]])
  // CHECK:   [[SELF:%.*]] = unchecked_ref_cast [[BASE_SELF_INIT]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[SELF]] to [init] [[PROJ]]
  //
  // Handle the return value.
  // CHECK:   [[SELF:%.*]] = load [copy] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   return [[SELF]]
  //
  // When the error is thrown, we need to:
  // 1. Store self back into the "conceptually" uninitialized box.
  // 2. destroy the box.
  // 3. Perform the rethrow.
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK:   [[SELF:%.*]] = unchecked_ref_cast [[BASE_SELF]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[SELF]] to [init] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   throw [[ERROR]]
  // CHECK: } // end sil function '$s21failable_initializers17ThrowDerivedClassC41delegatingFailDuringDelegationArgEmissionACSi_tKcfc'
  init(delegatingFailDuringDelegationArgEmission : Int) throws {
    super.init(noFail: try unwrap(delegatingFailDuringDelegationArgEmission))
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17ThrowDerivedClassC34delegatingFailDuringDelegationCallACSi_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error any Error) {
  // CHECK: bb0(
  // First initialize.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARK_UNINIT]]
  // CHECK:   [[PROJ:%.*]] = project_box [[LIFETIME]]
  // CHECK:   store {{%.*}} to [init] [[PROJ]]
  //
  // Call the initializer.
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[BASE_SELF:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[INIT_FN:%.*]] = function_ref @$s21failable_initializers14ThrowBaseClassCACyKcfc : $@convention(method)
  // CHECK:   try_apply [[INIT_FN]]([[BASE_SELF]]) : $@convention(method) (@owned ThrowBaseClass) -> (@owned ThrowBaseClass, @error any Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // Insert the return statement into the normal block...
  // CHECK: [[NORMAL_BB]]([[BASE_SELF_INIT:%.*]] : @owned $ThrowBaseClass):
  // CHECK:   [[OUT_SELF:%.*]] = unchecked_ref_cast [[BASE_SELF_INIT]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[OUT_SELF]] to [init] [[PROJ]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   return [[RESULT]]
  //
  // ... and destroy the box in the error block.
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK-NEXT:   end_borrow [[LIFETIME]]
  // CHECK-NEXT:   destroy_value [[MARK_UNINIT]]
  // CHECK-NEXT:   throw [[ERROR]]
  // CHECK: } // end sil function '$s21failable_initializers17ThrowDerivedClassC34delegatingFailDuringDelegationCallACSi_tKcfc'
  init(delegatingFailDuringDelegationCall : Int) throws {
    try super.init()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17ThrowDerivedClassC29delegatingFailAfterDelegationACSi_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error any Error) {
  // CHECK: bb0(
  // First initialize.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARK_UNINIT]]
  // CHECK:   [[PROJ:%.*]] = project_box [[LIFETIME]]
  // CHECK:   store {{%.*}} to [init] [[PROJ]]
  //
  // Call the initializer and then store the new self back into its memory slot.
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[BASE_SELF:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[INIT_FN:%.*]] = function_ref @$s21failable_initializers14ThrowBaseClassC6noFailACyt_tcfc : $@convention(method)
  // CHECK:   [[NEW_SELF:%.*]] = apply [[INIT_FN]]([[BASE_SELF]]) : $@convention(method) (@owned ThrowBaseClass) -> @owned ThrowBaseClass
  // CHECK:   [[NEW_SELF_CAST:%.*]] = unchecked_ref_cast [[NEW_SELF]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[NEW_SELF_CAST]] to [init] [[PROJ]]
  //
  // Finally perform the unwrap.
  // CHECK:   [[UNWRAP_FN:%.*]] = function_ref @$s21failable_initializers6unwrapyS2iKF : $@convention(thin) (Int) -> (Int, @error any Error)
  // CHECK:   try_apply [[UNWRAP_FN]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // Insert the return statement into the normal block...
  // CHECK: [[NORMAL_BB]](
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   return [[RESULT]]
  //
  // ... and destroy the box in the error block.
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK-NEXT:   end_borrow [[LIFETIME]]
  // CHECK-NEXT:   destroy_value [[MARK_UNINIT]]
  // CHECK-NEXT:   throw [[ERROR]]
  // CHECK: } // end sil function '$s21failable_initializers17ThrowDerivedClassC29delegatingFailAfterDelegationACSi_tKcfc'
  init(delegatingFailAfterDelegation : Int) throws {
    super.init(noFail: ())
    try unwrap(delegatingFailAfterDelegation)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17ThrowDerivedClassC30delegatingFailBeforeDelegation0fg6DuringI11ArgEmissionACSi_SitKcfc : $@convention(method) (Int, Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error any Error) {
  // Create our box.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[MARK_UNINIT_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[MARK_UNINIT]]
  // CHECK:   [[PROJ:%.*]] = project_box [[MARK_UNINIT_LIFETIME]]
  //
  // Perform the unwrap.
  // CHECK:   [[UNWRAP_FN:%.*]] = function_ref @$s21failable_initializers6unwrapyS2iKF : $@convention(thin) (Int) -> (Int, @error any Error)
  // CHECK:   try_apply [[UNWRAP_FN]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[UNWRAP_NORMAL_BB:bb[0-9]+]], error [[UNWRAP_ERROR_BB:bb[0-9]+]]
  //
  // Now we begin argument emission where we perform another unwrap.
  // CHECK: [[UNWRAP_NORMAL_BB]](
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[SELF_CAST:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[UNWRAP_FN2:%.*]] = function_ref @$s21failable_initializers6unwrapyS2iKF : $@convention(thin) (Int) -> (Int, @error any Error)
  // CHECK:   try_apply [[UNWRAP_FN2]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[UNWRAP_NORMAL_BB2:bb[0-9]+]], error [[UNWRAP_ERROR_BB2:bb[0-9]+]]
  //
  // Then since this example has a
  // CHECK: [[UNWRAP_NORMAL_BB2]]([[INT:%.*]] : $Int):
  // CHECK:   [[INIT_FN2:%.*]] = function_ref @$s21failable_initializers14ThrowBaseClassC6noFailACSi_tcfc : $@convention(method) (Int, @owned ThrowBaseClass) -> @owned ThrowBaseClass
  // CHECK:   [[NEW_SELF_CAST:%.*]] = apply [[INIT_FN2]]([[INT]], [[SELF_CAST]]) : $@convention(method) (Int, @owned ThrowBaseClass) -> @owned ThrowBaseClass
  // CHECK:   [[NEW_SELF:%.*]] = unchecked_ref_cast [[NEW_SELF_CAST]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[NEW_SELF]] to [init] [[PROJ]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PROJ]]
  // CHECK:   end_borrow [[MARK_UNINIT_LIFETIME]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   return [[RESULT]]
  //
  // ... and destroy the box in the error block.
  // CHECK: [[UNWRAP_ERROR_BB]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK:   br [[ERROR_JOIN:bb[0-9]+]]([[ERROR]]
  //
  // CHECK: [[UNWRAP_ERROR_BB2]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK:   [[SELF_CASTED_BACK:%.*]] = unchecked_ref_cast [[SELF_CAST]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[SELF_CASTED_BACK]] to [init] [[PROJ]]
  // CHECK:   br [[ERROR_JOIN]]([[ERROR]]
  //
  // CHECK: [[ERROR_JOIN]]([[ERROR_PHI:%.*]] : @owned $any Error):
  // CHECK:   end_borrow [[MARK_UNINIT_LIFETIME]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   throw [[ERROR_PHI]]
  // CHECK: } // end sil function '$s21failable_initializers17ThrowDerivedClassC30delegatingFailBeforeDelegation0fg6DuringI11ArgEmissionACSi_SitKcfc'
  init(delegatingFailBeforeDelegation : Int, delegatingFailDuringDelegationArgEmission : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    super.init(noFail: try unwrap(delegatingFailDuringDelegationArgEmission))
  }

  init(delegatingFailBeforeDelegation : Int, delegatingFailDuringDelegationCall : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    try super.init()
  }

  init(delegatingFailBeforeDelegation : Int, delegatingFailAfterDelegation : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    super.init(noFail: ())
    try unwrap(delegatingFailAfterDelegation)
  }

  init(delegatingFailDuringDelegationArgEmission : Int, delegatingFailDuringDelegationCall : Int) throws {
    try super.init(fail: try unwrap(delegatingFailDuringDelegationArgEmission))
  }

  init(delegatingFailDuringDelegationArgEmission : Int, delegatingFailAfterDelegation : Int) throws {
    super.init(noFail: try unwrap(delegatingFailDuringDelegationArgEmission))
    try unwrap(delegatingFailAfterDelegation)
  }

  init(delegatingFailDuringDelegationCall : Int, delegatingFailAfterDelegation : Int) throws {
    try super.init()
    try unwrap(delegatingFailAfterDelegation)
  }

  init(delegatingFailBeforeDelegation : Int, delegatingFailDuringDelegationArgEmission : Int, delegatingFailDuringDelegationCall : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    try super.init(fail: try unwrap(delegatingFailDuringDelegationArgEmission))
  }

  init(delegatingFailBeforeDelegation : Int, delegatingFailDuringDelegationArgEmission : Int, delegatingFailAfterDelegation : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    super.init(noFail: try unwrap(delegatingFailDuringDelegationArgEmission))
    try unwrap(delegatingFailAfterDelegation)
  }

  init(delegatingFailBeforeDelegation : Int, delegatingFailDuringDelegationCall : Int, delegatingFailAfterDelegation : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    try super.init()
    try unwrap(delegatingFailAfterDelegation)
  }

  init(delegatingFailDuringDelegationArgEmission : Int, delegatingFailDuringDelegationCall : Int, delegatingFailAfterDelegation : Int) throws {
    try super.init(fail: try unwrap(delegatingFailDuringDelegationArgEmission))
    try unwrap(delegatingFailAfterDelegation)
  }

  init(delegatingFailBeforeDelegation : Int, delegatingFailDuringDelegationArgEmission : Int, delegatingFailDuringDelegationCall : Int, delegatingFailAfterDelegation : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    try super.init(fail: try unwrap(delegatingFailDuringDelegationArgEmission))
    try unwrap(delegatingFailAfterDelegation)
  }

  // ---- Delegating to other self method.

  convenience init(chainingFailBeforeDelegation : Int) throws {
    try unwrap(chainingFailBeforeDelegation)
    self.init(noFail: ())
  }

  convenience init(chainingFailDuringDelegationArgEmission : Int) throws {
    self.init(noFail: try unwrap(chainingFailDuringDelegationArgEmission))
  }

  convenience init(chainingFailDuringDelegationCall : Int) throws {
    try self.init()
  }

  convenience init(chainingFailAfterDelegation : Int) throws {
    self.init(noFail: ())
    try unwrap(chainingFailAfterDelegation)
  }

  convenience init(chainingFailBeforeDelegation : Int, chainingFailDuringDelegationArgEmission : Int) throws {
    try unwrap(chainingFailBeforeDelegation)
    self.init(noFail: try unwrap(chainingFailDuringDelegationArgEmission))
  }

  convenience init(chainingFailBeforeDelegation : Int, chainingFailDuringDelegationCall : Int) throws {
    try unwrap(chainingFailBeforeDelegation)
    try self.init()
  }

  convenience init(chainingFailBeforeDelegation : Int, chainingFailAfterDelegation : Int) throws {
    try unwrap(chainingFailBeforeDelegation)
    self.init(noFail: ())
    try unwrap(chainingFailAfterDelegation)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17ThrowDerivedClassC39chainingFailDuringDelegationArgEmission0fghI4CallACSi_SitKcfC
  // CHECK: bb0({{.*}}, [[SELF_META:%.*]] : $@thick ThrowDerivedClass.Type):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[SELF_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_LIFETIME]]
  // CHECK:   try_apply {{.*}}({{.*}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[SUCC_BB1:bb[0-9]+]], error [[ERROR_BB1:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB1]](
  // CHECK:   try_apply {{.*}}({{.*}}, [[SELF_META]]) : {{.*}}, normal [[SUCC_BB2:bb[0-9]+]], error [[ERROR_BB2:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB2]]([[NEW_SELF:%.*]] : @owned $ThrowDerivedClass):
  // CHECK-NEXT: assign [[NEW_SELF]] to [[PB_BOX]]
  // CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[PB_BOX]]
  // CHECK-NEXT:   end_borrow [[SELF_LIFETIME]]
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: return [[RESULT]]
  //
  // CHECK: [[ERROR_BB1]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK-NEXT: br [[THROWING_BB:bb[0-9]+]]([[ERROR]]
  //
  // CHECK: [[ERROR_BB2]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK-NEXT: br [[THROWING_BB]]([[ERROR]]
  //
  // CHECK: [[THROWING_BB]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: throw [[ERROR]]
  convenience init(chainingFailDuringDelegationArgEmission : Int, chainingFailDuringDelegationCall : Int) throws {
    try self.init(fail: try unwrap(chainingFailDuringDelegationArgEmission))
  }

  convenience init(chainingFailDuringDelegationArgEmission : Int, chainingFailAfterDelegation : Int) throws {
    self.init(noFail: try unwrap(chainingFailDuringDelegationArgEmission))
    try unwrap(chainingFailAfterDelegation)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers17ThrowDerivedClassC32chainingFailDuringDelegationCall0fg5AfterI0ACSi_SitKcfC
  // CHECK: bb0({{.*}}, [[SELF_META:%.*]] : $@thick ThrowDerivedClass.Type):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[SELF_LIFETIME:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_LIFETIME]]
  // CHECK:   try_apply {{.*}}([[SELF_META]]) : {{.*}}, normal [[SUCC_BB1:bb[0-9]+]], error [[ERROR_BB1:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB1]]([[NEW_SELF:%.*]] : @owned $ThrowDerivedClass):
  // CHECK-NEXT:   assign [[NEW_SELF]] to [[PB_BOX]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   function_ref @
  // CHECK-NEXT:   try_apply {{.*}}({{.*}}) : $@convention(thin) (Int) -> (Int, @error any Error), normal [[SUCC_BB2:bb[0-9]+]], error [[ERROR_BB2:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB2]](
  // CHECK-NEXT: ignored_use
  // CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[PB_BOX]]
  // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: return [[RESULT]]
  //
  // CHECK: [[ERROR_BB1]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK-NEXT: br [[THROWING_BB:bb[0-9]+]]([[ERROR]]
  //
  // CHECK: [[ERROR_BB2]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK-NEXT: br [[THROWING_BB]]([[ERROR]]
  //
  // CHECK: [[THROWING_BB]]([[ERROR:%.*]] : @owned $any Error):
  // CHECK-NEXT: end_borrow [[SELF_LIFETIME]]
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: throw [[ERROR]]
  // CHECK: } // end sil function '$s21failable_initializers17ThrowDerivedClassC28chainingFailBeforeDelegation0fg6DuringI11ArgEmission0fgjI4CallACSi_S2itKcfC'
  convenience init(chainingFailDuringDelegationCall : Int, chainingFailAfterDelegation : Int) throws {
    try self.init()
    try unwrap(chainingFailAfterDelegation)
  }

  convenience init(chainingFailBeforeDelegation : Int, chainingFailDuringDelegationArgEmission : Int, chainingFailDuringDelegationCall : Int) throws {
    try unwrap(chainingFailBeforeDelegation)
    try self.init(fail: try unwrap(chainingFailDuringDelegationArgEmission))
  }

  convenience init(chainingFailBeforeDelegation : Int, chainingFailDuringDelegationArgEmission : Int, chainingFailAfterDelegation : Int) throws {
    try unwrap(chainingFailBeforeDelegation)
    self.init(noFail: try unwrap(chainingFailDuringDelegationArgEmission))
    try unwrap(chainingFailAfterDelegation)
  }

  convenience init(chainingFailBeforeDelegation : Int, chainingFailDuringDelegationCall : Int, chainingFailAfterDelegation : Int) throws {
    try unwrap(chainingFailBeforeDelegation)
    try self.init()
    try unwrap(chainingFailAfterDelegation)
  }

  convenience init(chainingFailDuringDelegationArgEmission : Int, chainingFailDuringDelegationCall : Int, chainingFailAfterDelegation : Int) throws {
    try self.init(fail: try unwrap(chainingFailDuringDelegationArgEmission))
    try unwrap(chainingFailAfterDelegation)
  }

  convenience init(chainingFailBeforeDelegation : Int, chainingFailDuringDelegationArgEmission : Int, chainingFailDuringDelegationCall : Int, chainingFailAfterDelegation : Int) throws {
    try unwrap(chainingFailBeforeDelegation)
    try self.init(fail: try unwrap(chainingFailDuringDelegationArgEmission))
    try unwrap(chainingFailAfterDelegation)
  }
}

////
// Enums with failable initializers
////

enum FailableEnum {
  case A

  init?(a: Int64) { self = .A }

  init!(b: Int64) {
    self.init(a: b)! // unnecessary-but-correct '!'
  }

  init(c: Int64) {
    self.init(a: c)! // necessary '!'
  }

  init(d: Int64) {
    self.init(b: d)! // unnecessary-but-correct '!'
  }
}

////
// Protocols and protocol extensions
////

// Delegating to failable initializers from a protocol extension to a
// protocol.
protocol P1 {
  init?(p1: Int64)
}

extension P1 {
  init!(p1a: Int64) {
    self.init(p1: p1a)! // unnecessary-but-correct '!'
  }

  init(p1b: Int64) {
    self.init(p1: p1b)! // necessary '!'
  }
}

protocol P2 : class {
  init?(p2: Int64)
}

extension P2 {
  init!(p2a: Int64) {
    self.init(p2: p2a)! // unnecessary-but-correct '!'
  }

  init(p2b: Int64) {
    self.init(p2: p2b)! // necessary '!'
  }
}

@objc protocol P3 {
  init?(p3: Int64)
}

extension P3 {
  init!(p3a: Int64) {
    self.init(p3: p3a)! // unnecessary-but-correct '!'
  }

  init(p3b: Int64) {
    self.init(p3: p3b)! // necessary '!'
  }
}

// Delegating to failable initializers from a protocol extension to a
// protocol extension.
extension P1 {
  init?(p1c: Int64) {
    self.init(p1: p1c)
  }

  init!(p1d: Int64) {
    self.init(p1c: p1d)! // unnecessary-but-correct '!'
  }

  init(p1e: Int64) {
    self.init(p1c: p1e)! // necessary '!'
  }
}

extension P2 {
  init?(p2c: Int64) {
    self.init(p2: p2c)
  }

  init!(p2d: Int64) {
    self.init(p2c: p2d)! // unnecessary-but-correct '!'
  }

  init(p2e: Int64) {
    self.init(p2c: p2e)! // necessary '!'
  }
}

////
// type(of: self) with uninitialized self
////

func use(_ a : Any) {}

class DynamicTypeBase {
  var x: Int

  init() {
    use(type(of: self))
    x = 0
  }

  convenience init(a : Int) {
    use(type(of: self))
    self.init()
  }
}

class DynamicTypeDerived : DynamicTypeBase {
  override init() {
    use(type(of: self))
    super.init()
  }

  convenience init(a : Int) {
    use(type(of: self))
    self.init()
  }
}

struct DynamicTypeStruct {
  var x: Int

  init() {
    use(type(of: self))
    x = 0
  }

  init(a : Int) {
    use(type(of: self))
    self.init()
  }
}

class InOutInitializer {
// CHECK-LABEL: sil hidden [exact_self_class] [ossa] @$s21failable_initializers16InOutInitializerC1xACSiz_tcfC : $@convention(method) (@inout Int, @thick InOutInitializer.Type) -> @owned InOutInitializer {
// CHECK: bb0(%0 : $*Int, %1 : $@thick InOutInitializer.Type):
  init(x: inout Int) {}
}

// <rdar://problem/16331406>
class SuperVariadic {
  init(ints: Int...) { }
}
class SubVariadic : SuperVariadic { }

// CHECK-LABEL: sil hidden [ossa] @$s21failable_initializers11SubVariadicC4intsACSid_tcfc
// CHECK:       bb0(%0 : @owned $Array<Int>, %1 : @owned $SubVariadic):
// CHECK:         [[SELF_UPCAST:%.*]] = upcast {{.*}} : $SubVariadic to $SuperVariadic
// CHECK:         [[T0:%.*]] = begin_borrow %0 : $Array<Int>
// CHECK:         [[T1:%.*]] = copy_value [[T0]] : $Array<Int>
// CHECK:         [[SUPER_INIT:%.*]] = function_ref @$s21failable_initializers13SuperVariadicC4intsACSid_tcfc
// CHECK:         apply [[SUPER_INIT]]([[T1]], [[SELF_UPCAST]])
// CHECK-LABEL: } // end sil function '$s21failable_initializers11SubVariadicC4intsACSid_tcfc'


public struct MemberInits<Value : Equatable> {
  private var box: MemberInitsHelper<Value>?
  fileprivate var value: String = "default"
}

class MemberInitsHelper<T> { }

extension MemberInits {
  // CHECK-LABEL: sil [ossa] @$s21failable_initializers11MemberInitsVyACySayqd__GGSayACyqd__GGcADRszSQRd__lufC : $@convention(method) <Value><T where Value == Array<T>, T : Equatable> (@owned Array<MemberInits<T>>, @thin MemberInits<Array<T>>.Type) -> @owned MemberInits<Array<T>> {
  public init<T>(_ array: Array<MemberInits<T>>) where Value == Array<T> {
    box = nil

    // CHECK: [[INIT_FN:%.*]] = function_ref @$s21failable_initializers11MemberInitsV5value33_4497B2E9306011E5BAC13C07BEAC2557LLSSvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : Equatable> () -> @owned String
    // CHECK-NEXT:  = apply [[INIT_FN]]<Array<T>>() : $@convention(thin) <τ_0_0 where τ_0_0 : Equatable> () -> @owned String
  }
}

// rdar://problem/51302498

class Butt {
  init(foo: inout (Int, Int)) { }
}

