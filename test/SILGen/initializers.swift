// RUN: %target-swift-emit-silgen -enable-sil-ownership -disable-objc-attr-requires-foundation-module -enable-objc-interop %s -module-name failable_initializers | %FileCheck %s

// High-level tests that silgen properly emits code for failable and thorwing
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

  init(fail: ()) throws { x = Canary() }

  init(noFail: ()) { x = Canary() }

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
    try! self.init(throwsToOptionalThrows: throwsOptionalToOptional)
  }

  init(failBeforeSelfReplacement: Int) throws {
    try unwrap(failBeforeSelfReplacement)
    self = ThrowStruct(noFail: ())
  }

  init(failDuringSelfReplacement: Int) throws {
    try self = ThrowStruct(fail: ())
  }

  init(failAfterSelfReplacement: Int) throws {
    self = ThrowStruct(noFail: ())
    try unwrap(failAfterSelfReplacement)
  }
}

extension ThrowStruct {
  init(failInExtension: ()) throws {
    try self.init(fail: failInExtension)
  }

  init(assignInExtension: ()) throws {
    try self = ThrowStruct(fail: ())
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

  // CHECK-LABEL: sil hidden @$S21failable_initializers17FailableBaseClassC19failAfterDelegationACSgyt_tcfc : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : @owned $FailableBaseClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[PB_BOX]]
  // CHECK:   [[TAKE_SELF:%.*]] = load [take] [[PB_BOX]]
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[TAKE_SELF]]) : $@convention(method) (@owned FailableBaseClass) -> @owned FailableBaseClass
  // CHECK:   destroy_value [[MARKED_SELF_BOX]]
  // CHECK:   [[RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.none!enumelt
  // CHECK:   br bb2([[RESULT]] : $Optional<FailableBaseClass>)
  // CHECK: bb2([[RESULT:%.*]] : @owned $Optional<FailableBaseClass>):
  // CHECK:   return [[RESULT]]
  // CHECK: } // end sil function '$S21failable_initializers17FailableBaseClassC19failAfterDelegationACSgyt_tcfc
  convenience init?(failAfterDelegation: ()) {
    self.init(noFail: ())
    return nil
  }

  // Optional to optional
  //
  // CHECK-LABEL: sil hidden @$S21failable_initializers17FailableBaseClassC20failDuringDelegationACSgyt_tcfc : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : @owned $FailableBaseClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[PB_BOX]]
  // CHECK:   [[TAKE_SELF:%.*]] = load [take] [[PB_BOX]]
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[TAKE_SELF]]) : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass>
  // CHECK:   cond_br {{.*}}, [[SUCC_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[FAIL_BB:bb[0-9]+]]:
  // CHECK:   destroy_value [[NEW_SELF]]
  // CHECK:   br [[FAIL_EPILOG_BB:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB]]:
  // CHECK:   [[UNWRAPPED_NEW_SELF:%.*]] = unchecked_enum_data [[NEW_SELF]] : $Optional<FailableBaseClass>, #Optional.some!enumelt.1
  // CHECK:   store [[UNWRAPPED_NEW_SELF]] to [init] [[PB_BOX]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PB_BOX]]
  // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.some!enumelt.1, [[RESULT]]
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
  // CHECK-LABEL: sil hidden @$S21failable_initializers17FailableBaseClassC21failDuringDelegation2ACSgyt_tcfc : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : @owned $FailableBaseClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[PB_BOX]]
  // CHECK-NEXT:   [[TAKE_SELF:%.*]] = load [take] [[PB_BOX]]
  // CHECK-NOT: [[OLD_SELF]]
  // CHECK-NOT: copy_value
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[TAKE_SELF]]) : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass>
  // CHECK:   switch_enum [[NEW_SELF]] : $Optional<FailableBaseClass>, case #Optional.some!enumelt.1: [[SUCC_BB:bb[0-9]+]], case #Optional.none!enumelt: [[FAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[FAIL_BB]]:
  // CHECK:   unreachable
  //
  // CHECK: [[SUCC_BB]]([[RESULT:%.*]] : @owned $FailableBaseClass):
  // CHECK:   store [[RESULT]] to [init] [[PB_BOX]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PB_BOX]]
  // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.some!enumelt.1, [[RESULT]] : $FailableBaseClass
  // CHECK:   destroy_value [[MARKED_SELF_BOX]]
  // CHECK:   br [[EPILOG_BB:bb[0-9]+]]([[WRAPPED_RESULT]]
  //
  // CHECK: [[EPILOG_BB]]([[WRAPPED_RESULT:%.*]] : @owned $Optional<FailableBaseClass>):
  // CHECK:   return [[WRAPPED_RESULT]]
  // CHECK: } // end sil function '$S21failable_initializers17FailableBaseClassC21failDuringDelegation2ACSgyt_tcfc'
  convenience init!(failDuringDelegation2: ()) {
    self.init(failBeforeFullInitialization: ())! // unnecessary-but-correct '!'
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

  // CHECK-LABEL: sil hidden @$S21failable_initializers20FailableDerivedClassC27derivedFailBeforeDelegationACSgyt_tcfc : $@convention(method) (@owned FailableDerivedClass) -> @owned Optional<FailableDerivedClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : @owned $FailableDerivedClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableDerivedClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [derivedself] [[SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[PB_BOX]]
  // CHECK-NEXT: br bb1
  //
  // CHECK: bb1:
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: [[RESULT:%.*]] = enum $Optional<FailableDerivedClass>, #Optional.none!enumelt
  // CHECK-NEXT: br bb2([[RESULT]]
  //
  // CHECK: bb2([[RESULT:%.*]] : @owned $Optional<FailableDerivedClass>):
  // CHECK-NEXT: return [[RESULT]]
  init?(derivedFailBeforeDelegation: ()) {
    return nil
  }

  // CHECK-LABEL: sil hidden @$S21failable_initializers20FailableDerivedClassC27derivedFailDuringDelegationACSgyt_tcfc : $@convention(method) (@owned FailableDerivedClass) -> @owned Optional<FailableDerivedClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : @owned $FailableDerivedClass):
  init?(derivedFailDuringDelegation: ()) {
    // First initialize the lvalue for self.
    // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableDerivedClass }, let, name "self"
    // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [derivedself] [[SELF_BOX]]
    // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
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
    // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableDerivedClass>, #Optional.some!enumelt.1, [[RESULT]]
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

  // CHECK-LABEL: sil hidden @$S21failable_initializers17ThrowDerivedClassC30delegatingFailBeforeDelegationACSi_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error) {
  // CHECK: bb0(
  // First initialize.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[PROJ:%.*]] = project_box [[MARK_UNINIT]]
  // CHECK:   store {{%.*}} to [init] [[PROJ]]
  //
  // Then initialize the canary with nil. We are able to borrow the initialized self to avoid retain/release overhead.
  // CHECK:   [[CANARY_FUNC:%.*]] = function_ref @$S21failable_initializers17ThrowDerivedClassC6canaryAA6CanaryCSgvpfi :
  // CHECK:   [[OPT_CANARY:%.*]] = apply [[CANARY_FUNC]]()
  // CHECK:   [[SELF:%.*]] = load_borrow [[PROJ]]
  // CHECK:   [[CANARY_ADDR:%.*]] = ref_element_addr [[SELF]]
  // CHECK:   [[CANARY_ACCESS:%.*]] = begin_access [modify] [dynamic] [[CANARY_ADDR]]
  // CHECK:   assign [[OPT_CANARY]] to [[CANARY_ACCESS]]
  // CHECK:   end_access [[CANARY_ACCESS]]
  // CHECK:   end_borrow [[SELF]] from [[PROJ]]
  //
  // Now we perform the unwrap.
  // CHECK:   [[UNWRAP_FN:%.*]] = function_ref @$S21failable_initializers6unwrapyS2iKF : $@convention(thin)
  // CHECK:   try_apply [[UNWRAP_FN]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // CHECK: [[NORMAL_BB]](
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[SELF_BASE:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[BASE_INIT_FN:%.*]] = function_ref @$S21failable_initializers14ThrowBaseClassC6noFailACyt_tcfc : $@convention(method)
  // CHECK:   [[SELF_INIT_BASE:%.*]] = apply [[BASE_INIT_FN]]([[SELF_BASE]])
  // CHECK:   [[SELF:%.*]] = unchecked_ref_cast [[SELF_INIT_BASE]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[SELF]] to [init] [[PROJ]]
  // CHECK:   [[SELF:%.*]] = load [copy] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   return [[SELF]]
  //
  // Finally the error BB. We do not touch self since self is still in the
  // box implying that destroying MARK_UNINIT will destroy it for us.
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $Error):
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   throw [[ERROR]]
  // CHECK: } // end sil function '$S21failable_initializers17ThrowDerivedClassC30delegatingFailBeforeDelegationACSi_tKcfc'
  init(delegatingFailBeforeDelegation : Int) throws {
    try unwrap(delegatingFailBeforeDelegation)
    super.init(noFail: ())
  }

  // CHECK-LABEL: sil hidden @$S21failable_initializers17ThrowDerivedClassC41delegatingFailDuringDelegationArgEmissionACSi_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error) {
  // CHECK: bb0(
  // First initialize.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[PROJ:%.*]] = project_box [[MARK_UNINIT]]
  // CHECK:   store {{%.*}} to [init] [[PROJ]]
  //
  // Then initialize the canary with nil. We are able to borrow the initialized self to avoid retain/release overhead.
  // CHECK:   [[CANARY_FUNC:%.*]] = function_ref @$S21failable_initializers17ThrowDerivedClassC6canaryAA6CanaryCSgvpfi :
  // CHECK:   [[OPT_CANARY:%.*]] = apply [[CANARY_FUNC]]()
  // CHECK:   [[SELF:%.*]] = load_borrow [[PROJ]]
  // CHECK:   [[CANARY_ADDR:%.*]] = ref_element_addr [[SELF]]
  // CHECK:   [[CANARY_ACCESS:%.*]] = begin_access [modify] [dynamic] [[CANARY_ADDR]]
  // CHECK:   assign [[OPT_CANARY]] to [[CANARY_ACCESS]]
  // CHECK:   end_access [[CANARY_ACCESS]]
  // CHECK:   end_borrow [[SELF]] from [[PROJ]]
  //
  // Now we begin argument emission where we perform the unwrap.
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[BASE_SELF:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[UNWRAP_FN:%.*]] = function_ref @$S21failable_initializers6unwrapyS2iKF : $@convention(thin)
  // CHECK:   try_apply [[UNWRAP_FN]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // Now we emit the call to the initializer. Notice how we return self back to
  // its memory locatio nbefore any other work is done.
  // CHECK: [[NORMAL_BB]](
  // CHECK:   [[INIT_FN:%.*]] = function_ref @$S21failable_initializers14ThrowBaseClassC6noFailACSi_tcfc : $@convention(method)
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
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $Error):
  // CHECK:   [[SELF:%.*]] = unchecked_ref_cast [[BASE_SELF]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[SELF]] to [init] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   throw [[ERROR]]
  // CHECK: } // end sil function '$S21failable_initializers17ThrowDerivedClassC41delegatingFailDuringDelegationArgEmissionACSi_tKcfc'
  init(delegatingFailDuringDelegationArgEmission : Int) throws {
    super.init(noFail: try unwrap(delegatingFailDuringDelegationArgEmission))
  }

  // CHECK-LABEL: sil hidden @$S21failable_initializers17ThrowDerivedClassC34delegatingFailDuringDelegationCallACSi_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error) {
  // CHECK: bb0(
  // First initialize.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[PROJ:%.*]] = project_box [[MARK_UNINIT]]
  // CHECK:   store {{%.*}} to [init] [[PROJ]]
  //
  // Call the initializer.
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[BASE_SELF:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[INIT_FN:%.*]] = function_ref @$S21failable_initializers14ThrowBaseClassCACyKcfc : $@convention(method)
  // CHECK:   try_apply [[INIT_FN]]([[BASE_SELF]]) : $@convention(method) (@owned ThrowBaseClass) -> (@owned ThrowBaseClass, @error Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
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
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $Error):
  // CHECK-NEXT:   destroy_value [[MARK_UNINIT]]
  // CHECK-NEXT:   throw [[ERROR]]
  // CHECK: } // end sil function '$S21failable_initializers17ThrowDerivedClassC34delegatingFailDuringDelegationCallACSi_tKcfc'
  init(delegatingFailDuringDelegationCall : Int) throws {
    try super.init()
  }

  // CHECK-LABEL: sil hidden @$S21failable_initializers17ThrowDerivedClassC29delegatingFailAfterDelegationACSi_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error) {
  // CHECK: bb0(
  // First initialize.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[PROJ:%.*]] = project_box [[MARK_UNINIT]]
  // CHECK:   store {{%.*}} to [init] [[PROJ]]
  //
  // Call the initializer and then store the new self back into its memory slot.
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[BASE_SELF:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[INIT_FN:%.*]] = function_ref @$S21failable_initializers14ThrowBaseClassC6noFailACyt_tcfc : $@convention(method)
  // CHECK:   [[NEW_SELF:%.*]] = apply [[INIT_FN]]([[BASE_SELF]]) : $@convention(method) (@owned ThrowBaseClass) -> @owned ThrowBaseClass
  // CHECK:   [[NEW_SELF_CAST:%.*]] = unchecked_ref_cast [[NEW_SELF]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[NEW_SELF_CAST]] to [init] [[PROJ]]
  //
  // Finally perform the unwrap.
  // CHECK:   [[UNWRAP_FN:%.*]] = function_ref @$S21failable_initializers6unwrapyS2iKF : $@convention(thin) (Int) -> (Int, @error Error)
  // CHECK:   try_apply [[UNWRAP_FN]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
  //
  // Insert the return statement into the normal block...
  // CHECK: [[NORMAL_BB]](
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   return [[RESULT]]
  //
  // ... and destroy the box in the error block.
  // CHECK: [[ERROR_BB]]([[ERROR:%.*]] : @owned $Error):
  // CHECK-NEXT:   destroy_value [[MARK_UNINIT]]
  // CHECK-NEXT:   throw [[ERROR]]
  // CHECK: } // end sil function '$S21failable_initializers17ThrowDerivedClassC29delegatingFailAfterDelegationACSi_tKcfc'
  init(delegatingFailAfterDelegation : Int) throws {
    super.init(noFail: ())
    try unwrap(delegatingFailAfterDelegation)
  }

  // CHECK-LABEL: sil hidden @$S21failable_initializers17ThrowDerivedClassC30delegatingFailBeforeDelegation0fg6DuringI11ArgEmissionACSi_SitKcfc : $@convention(method) (Int, Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error) {
  // Create our box.
  // CHECK:   [[REF:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARK_UNINIT:%.*]] = mark_uninitialized [derivedself] [[REF]] : ${ var ThrowDerivedClass }
  // CHECK:   [[PROJ:%.*]] = project_box [[MARK_UNINIT]]
  //
  // Perform the unwrap.
  // CHECK:   [[UNWRAP_FN:%.*]] = function_ref @$S21failable_initializers6unwrapyS2iKF : $@convention(thin) (Int) -> (Int, @error Error)
  // CHECK:   try_apply [[UNWRAP_FN]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[UNWRAP_NORMAL_BB:bb[0-9]+]], error [[UNWRAP_ERROR_BB:bb[0-9]+]]
  //
  // Now we begin argument emission where we perform another unwrap.
  // CHECK: [[UNWRAP_NORMAL_BB]](
  // CHECK:   [[SELF:%.*]] = load [take] [[PROJ]]
  // CHECK:   [[SELF_CAST:%.*]] = upcast [[SELF]] : $ThrowDerivedClass to $ThrowBaseClass
  // CHECK:   [[UNWRAP_FN2:%.*]] = function_ref @$S21failable_initializers6unwrapyS2iKF : $@convention(thin) (Int) -> (Int, @error Error)
  // CHECK:   try_apply [[UNWRAP_FN2]]({{%.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[UNWRAP_NORMAL_BB2:bb[0-9]+]], error [[UNWRAP_ERROR_BB2:bb[0-9]+]]
  //
  // Then since this example has a
  // CHECK: [[UNWRAP_NORMAL_BB2]]([[INT:%.*]] : @trivial $Int):
  // CHECK:   [[INIT_FN2:%.*]] = function_ref @$S21failable_initializers14ThrowBaseClassC6noFailACSi_tcfc : $@convention(method) (Int, @owned ThrowBaseClass) -> @owned ThrowBaseClass
  // CHECK:   [[NEW_SELF_CAST:%.*]] = apply [[INIT_FN2]]([[INT]], [[SELF_CAST]]) : $@convention(method) (Int, @owned ThrowBaseClass) -> @owned ThrowBaseClass
  // CHECK:   [[NEW_SELF:%.*]] = unchecked_ref_cast [[NEW_SELF_CAST]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[NEW_SELF]] to [init] [[PROJ]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[PROJ]]
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   return [[RESULT]]
  //
  // ... and destroy the box in the error block.
  // CHECK: [[UNWRAP_ERROR_BB]]([[ERROR:%.*]] : @owned $Error):
  // CHECK:   br [[ERROR_JOIN:bb[0-9]+]]([[ERROR]]
  //
  // CHECK: [[UNWRAP_ERROR_BB2]]([[ERROR:%.*]] : @owned $Error):
  // CHECK:   [[SELF_CASTED_BACK:%.*]] = unchecked_ref_cast [[SELF_CAST]] : $ThrowBaseClass to $ThrowDerivedClass
  // CHECK:   store [[SELF_CASTED_BACK]] to [init] [[PROJ]]
  // CHECK:   br [[ERROR_JOIN]]([[ERROR]]
  //
  // CHECK: [[ERROR_JOIN]]([[ERROR_PHI:%.*]] : @owned $Error):
  // CHECK:   destroy_value [[MARK_UNINIT]]
  // CHECK:   throw [[ERROR_PHI]]
  // CHECK: } // end sil function '$S21failable_initializers17ThrowDerivedClassC30delegatingFailBeforeDelegation0fg6DuringI11ArgEmissionACSi_SitKcfc'
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

  // CHECK-LABEL: sil hidden @$S21failable_initializers17ThrowDerivedClassC39chainingFailDuringDelegationArgEmission0fghI4CallACSi_SitKcfc : $@convention(method) (Int, Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error) {
  // CHECK: bb0({{.*}}, [[OLD_SELF:%.*]] : @owned $ThrowDerivedClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[PB_BOX]]
  // CHECK:   [[MOVE_ONLY_SELF:%.*]] = load [take] [[PB_BOX]]
  // CHECK:   try_apply {{.*}}({{.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[SUCC_BB1:bb[0-9]+]], error [[ERROR_BB1:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB1]](
  // CHECK:   try_apply {{.*}}({{.*}}, [[MOVE_ONLY_SELF]]) : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error), normal [[SUCC_BB2:bb[0-9]+]], error [[ERROR_BB2:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB2]]([[NEW_SELF:%.*]] : @owned $ThrowDerivedClass):
  // CHECK-NEXT: store [[NEW_SELF]] to [init] [[PB_BOX]]
  // CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[PB_BOX]]
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: return [[RESULT]]
  //
  // CHECK: [[ERROR_BB1]]([[ERROR:%.*]] : @owned $Error):
  // CHECK-NEXT: store [[MOVE_ONLY_SELF]] to [init] [[PB_BOX]]
  // CHECK-NEXT: br [[THROWING_BB:bb[0-9]+]]([[ERROR]]
  //
  // CHECK: [[ERROR_BB2]]([[ERROR:%.*]] : @owned $Error):
  // CHECK-NEXT: br [[THROWING_BB]]([[ERROR]]
  //
  // CHECK: [[THROWING_BB]]([[ERROR:%.*]] : @owned $Error):
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: throw [[ERROR]]
  convenience init(chainingFailDuringDelegationArgEmission : Int, chainingFailDuringDelegationCall : Int) throws {
    try self.init(fail: try unwrap(chainingFailDuringDelegationArgEmission))
  }

  convenience init(chainingFailDuringDelegationArgEmission : Int, chainingFailAfterDelegation : Int) throws {
    self.init(noFail: try unwrap(chainingFailDuringDelegationArgEmission))
    try unwrap(chainingFailAfterDelegation)
  }

  // CHECK-LABEL: sil hidden @$S21failable_initializers17ThrowDerivedClassC32chainingFailDuringDelegationCall0fg5AfterI0ACSi_SitKcfc : $@convention(method) (Int, Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error) {
  // CHECK: bb0({{.*}}, [[OLD_SELF:%.*]] : @owned $ThrowDerivedClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[MARKED_SELF_BOX:%.*]] = mark_uninitialized [delegatingself] [[SELF_BOX]]
  // CHECK:   [[PB_BOX:%.*]] = project_box [[MARKED_SELF_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[PB_BOX]]
  // CHECK:   [[MOVE_ONLY_SELF:%.*]] = load [take] [[PB_BOX]]
  // CHECK:   try_apply {{.*}}([[MOVE_ONLY_SELF]]) : $@convention(method) (@owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error), normal [[SUCC_BB1:bb[0-9]+]], error [[ERROR_BB1:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB1]]([[NEW_SELF:%.*]] : @owned $ThrowDerivedClass):
  // CHECK-NEXT:   store [[NEW_SELF]] to [init] [[PB_BOX]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   function_ref @
  // CHECK-NEXT:   try_apply {{.*}}({{.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[SUCC_BB2:bb[0-9]+]], error [[ERROR_BB2:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB2]](
  // CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[PB_BOX]]
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: return [[RESULT]]
  //
  // CHECK: [[ERROR_BB1]]([[ERROR:%.*]] : @owned $Error):
  // CHECK-NEXT: br [[THROWING_BB:bb[0-9]+]]([[ERROR]]
  //
  // CHECK: [[ERROR_BB2]]([[ERROR:%.*]] : @owned $Error):
  // CHECK-NEXT: br [[THROWING_BB]]([[ERROR]]
  //
  // CHECK: [[THROWING_BB]]([[ERROR:%.*]] : @owned $Error):
  // CHECK-NEXT: destroy_value [[MARKED_SELF_BOX]]
  // CHECK-NEXT: throw [[ERROR]]
  // CHECK: } // end sil function '$S21failable_initializers17ThrowDerivedClassC28chainingFailBeforeDelegation0fg6DuringI11ArgEmission0fgjI4CallACSi_S2itKcfC'
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
// CHECK-LABEL: sil hidden @$S21failable_initializers16InOutInitializerC1xACSiz_tcfC : $@convention(method) (@inout Int, @thick InOutInitializer.Type) -> @owned InOutInitializer {
// CHECK: bb0(%0 : @trivial $*Int, %1 : @trivial $@thick InOutInitializer.Type):
  init(x: inout Int) {}
}

// <rdar://problem/16331406>
class SuperVariadic {
  init(ints: Int...) { }
}
class SubVariadic : SuperVariadic { }

// CHECK-LABEL: sil hidden @$S21failable_initializers11SubVariadicC4intsACSid_tcfc
// CHECK:       bb0(%0 : @owned $Array<Int>, %1 : @owned $SubVariadic):
// CHECK:         [[SELF_UPCAST:%.*]] = upcast {{.*}} : $SubVariadic to $SuperVariadic
// CHECK:         [[T0:%.*]] = begin_borrow %0 : $Array<Int>
// CHECK:         [[T1:%.*]] = copy_value [[T0]] : $Array<Int>
// CHECK:         [[SUPER_INIT:%.*]] = function_ref @$S21failable_initializers13SuperVariadicC4intsACSid_tcfc
// CHECK:         apply [[SUPER_INIT]]([[T1]], [[SELF_UPCAST]])
// CHECK-LABEL: } // end sil function
