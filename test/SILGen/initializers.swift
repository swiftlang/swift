// RUN: %target-swift-frontend -emit-silgen -disable-objc-attr-requires-foundation-module %s -module-name failable_initializers | %FileCheck %s

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

  // CHECK-LABEL: sil hidden @_T021failable_initializers17FailableBaseClassCACSgyt19failAfterDelegation_tcfc : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : $FailableBaseClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_BOX]]
  // CHECK:   [[MUI:%.*]] = mark_uninitialized [delegatingself] [[PB_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[MUI]]
  // CHECK:   [[TAKE_SELF:%.*]] = load [take] [[MUI]]
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[TAKE_SELF]]) : $@convention(method) (@owned FailableBaseClass) -> @owned FailableBaseClass
  // CHECK:   destroy_value [[SELF_BOX]]
  // CHECK:   [[RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.none!enumelt
  // CHECK:   br bb2([[RESULT]] : $Optional<FailableBaseClass>)
  // CHECK: bb2([[RESULT:%.*]] : $Optional<FailableBaseClass>):
  // CHECK:   return [[RESULT]]
  // CHECK: } // end sil function '_T021failable_initializers17FailableBaseClassCACSgyt19failAfterDelegation_tcfc
  convenience init?(failAfterDelegation: ()) {
    self.init(noFail: ())
    return nil
  }

  // Optional to optional
  //
  // CHECK-LABEL: sil hidden @_T021failable_initializers17FailableBaseClassCACSgyt20failDuringDelegation_tcfc : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : $FailableBaseClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_BOX]]
  // CHECK:   [[MUI:%.*]] = mark_uninitialized [delegatingself] [[PB_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[MUI]]
  // CHECK:   [[TAKE_SELF:%.*]] = load [take] [[MUI]]
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[TAKE_SELF]]) : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass>
  // CHECK:   cond_br {{.*}}, [[SUCC_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[FAIL_BB:bb[0-9]+]]:
  // CHECK:   destroy_value [[NEW_SELF]]
  // CHECK:   br [[FAIL_EPILOG_BB:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB]]:
  // CHECK:   [[UNWRAPPED_NEW_SELF:%.*]] = unchecked_enum_data [[NEW_SELF]] : $Optional<FailableBaseClass>, #Optional.some!enumelt.1
  // CHECK:   store [[UNWRAPPED_NEW_SELF]] to [init] [[MUI]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[MUI]]
  // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.some!enumelt.1, [[RESULT]]
  // CHECK:   destroy_value [[SELF_BOX]]
  // CHECK:   br [[EPILOG_BB:bb[0-9]+]]([[WRAPPED_RESULT]]
  //
  // CHECK: [[FAIL_EPILOG_BB]]:
  // CHECK:   destroy_value [[SELF_BOX]]
  // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.none!enumelt
  // CHECK:   br [[EPILOG_BB]]([[WRAPPED_RESULT]]
  //
  // CHECK: [[EPILOG_BB]]([[WRAPPED_RESULT:%.*]] : $Optional<FailableBaseClass>):
  // CHECK:   return [[WRAPPED_RESULT]]
  convenience init?(failDuringDelegation: ()) {
    self.init(failBeforeFullInitialization: ())
  }

  // IUO to optional
  //
  // CHECK-LABEL: sil hidden @_T021failable_initializers17FailableBaseClassCSQyACGyt21failDuringDelegation2_tcfc : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : $FailableBaseClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableBaseClass }, let, name "self"
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_BOX]]
  // CHECK:   [[MUI:%.*]] = mark_uninitialized [delegatingself] [[PB_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[MUI]]
  // CHECK-NEXT:   [[TAKE_SELF:%.*]] = load [take] [[MUI]]
  // CHECK-NOT: [[OLD_SELF]]
  // CHECK-NOT: copy_value
  // CHECK:   [[NEW_SELF:%.*]] = apply {{.*}}([[TAKE_SELF]]) : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass>
  // CHECK:   switch_enum [[NEW_SELF]] : $Optional<FailableBaseClass>, case #Optional.some!enumelt.1: [[SUCC_BB:bb[0-9]+]], case #Optional.none!enumelt: [[FAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[FAIL_BB]]:
  // CHECK:   unreachable
  //
  // CHECK: [[SUCC_BB]]([[RESULT:%.*]] : $FailableBaseClass):
  // CHECK:   store [[RESULT]] to [init] [[MUI]]
  // CHECK:   [[RESULT:%.*]] = load [copy] [[MUI]]
  // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableBaseClass>, #Optional.some!enumelt.1, [[RESULT]] : $FailableBaseClass
  // CHECK:   destroy_value [[SELF_BOX]]
  // CHECK:   br [[EPILOG_BB:bb[0-9]+]]([[WRAPPED_RESULT]]
  //
  // CHECK: [[EPILOG_BB]]([[WRAPPED_RESULT:%.*]] : $Optional<FailableBaseClass>):
  // CHECK:   return [[WRAPPED_RESULT]]
  // CHECK: } // end sil function '_T021failable_initializers17FailableBaseClassCSQyACGyt21failDuringDelegation2_tcfc'
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

  // CHECK-LABEL: sil hidden @_T021failable_initializers20FailableDerivedClassCACSgyt27derivedFailBeforeDelegation_tcfc : $@convention(method) (@owned FailableDerivedClass) -> @owned Optional<FailableDerivedClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : $FailableDerivedClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableDerivedClass }, let, name "self"
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_BOX]]
  // CHECK:   [[MUI:%.*]] = mark_uninitialized [derivedself] [[PB_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[MUI]]
  // CHECK-NEXT: br bb1
  //
  // CHECK: bb1:
  // CHECK-NEXT: destroy_value [[SELF_BOX]]
  // CHECK-NEXT: [[RESULT:%.*]] = enum $Optional<FailableDerivedClass>, #Optional.none!enumelt
  // CHECK-NEXT: br bb2([[RESULT]]
  //
  // CHECK: bb2([[RESULT:%.*]] : $Optional<FailableDerivedClass>):
  // CHECK-NEXT: return [[RESULT]]
  init?(derivedFailBeforeDelegation: ()) {
    return nil
  }

  // CHECK-LABEL: sil hidden @_T021failable_initializers20FailableDerivedClassCACSgyt27derivedFailDuringDelegation_tcfc : $@convention(method) (@owned FailableDerivedClass) -> @owned Optional<FailableDerivedClass> {
  // CHECK: bb0([[OLD_SELF:%.*]] : $FailableDerivedClass):
  init?(derivedFailDuringDelegation: ()) {
    // First initialize the lvalue for self.
    // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var FailableDerivedClass }, let, name "self"
    // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_BOX]]
    // CHECK:   [[MUI:%.*]] = mark_uninitialized [derivedself] [[PB_BOX]]
    // CHECK:   store [[OLD_SELF]] to [init] [[MUI]]
    //
    // Then assign canary to other member using a borrow.
    // CHECK:   [[BORROWED_SELF:%.*]] = load_borrow [[MUI]]
    // CHECK:   [[CANARY_VALUE:%.*]] = apply
    // CHECK:   [[CANARY_GEP:%.*]] = ref_element_addr [[BORROWED_SELF]]
    // CHECK:   assign [[CANARY_VALUE]] to [[CANARY_GEP]]
    self.otherMember = Canary()

    // Finally, begin the super init sequence.
    // CHECK:   [[LOADED_SELF:%.*]] = load [take] [[MUI]]
    // CHECK:   [[UPCAST_SELF:%.*]] = upcast [[LOADED_SELF]]
    // CHECK:   [[OPT_NEW_SELF:%.*]] = apply {{.*}}([[UPCAST_SELF]]) : $@convention(method) (@owned FailableBaseClass) -> @owned Optional<FailableBaseClass>
    // CHECK:   [[IS_NIL:%.*]] = select_enum [[OPT_NEW_SELF]]
    // CHECK:   cond_br [[IS_NIL]], [[SUCC_BB:bb[0-9]+]], [[FAIL_BB:bb[0-9]+]]
    //
    // And then return nil making sure that we do not insert any extra values anywhere.
    // CHECK: [[SUCC_BB]]:
    // CHECK:   [[NEW_SELF:%.*]] = unchecked_enum_data [[OPT_NEW_SELF]]
    // CHECK:   [[DOWNCAST_NEW_SELF:%.*]] = unchecked_ref_cast [[NEW_SELF]]
    // CHECK:   store [[DOWNCAST_NEW_SELF]] to [init] [[MUI]]
    // CHECK:   [[RESULT:%.*]] = load [copy] [[MUI]]
    // CHECK:   [[WRAPPED_RESULT:%.*]] = enum $Optional<FailableDerivedClass>, #Optional.some!enumelt.1, [[RESULT]]
    // CHECK:   destroy_value [[SELF_BOX]]
    // CHECK:   br [[EPILOG_BB:bb[0-9]+]]([[WRAPPED_RESULT]]
    //
    // CHECK: [[FAIL_BB]]:
    // CHECK:   destroy_value [[SELF_BOX]]
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

  init(fail: Int)  {}

  init(failBeforeFullInitialization: Int) throws {
    try unwrap(failBeforeFullInitialization)
    super.init(noFail: ())
  }

  init(failBeforeFullInitialization: Int, failDuringFullInitialization: Int) throws {
    try unwrap(failBeforeFullInitialization)
    try super.init()
  }

  init(failAfterFullInitialization: Int) throws {
    super.init(noFail: ())
    try unwrap(failAfterFullInitialization)
  }

  init(failAfterFullInitialization: Int, failDuringFullInitialization: Int) throws {
    try super.init()
    try unwrap(failAfterFullInitialization)
  }

  init(failBeforeFullInitialization: Int, failAfterFullInitialization: Int) throws {
    try unwrap(failBeforeFullInitialization)
    super.init(noFail: ())
    try unwrap(failAfterFullInitialization)
  }

  init(failBeforeFullInitialization: Int, failDuringFullInitialization: Int, failAfterFullInitialization: Int) throws {
    try unwrap(failBeforeFullInitialization)
    try super.init()
    try unwrap(failAfterFullInitialization)
  }

  convenience init(noFail2: ()) {
    try! self.init()
  }

  convenience init(failBeforeDelegation: Int) throws {
    try unwrap(failBeforeDelegation)
    self.init(noFail: ())
  }

  convenience init(failDuringDelegation: Int) throws {
    try self.init()
  }

  convenience init(failBeforeOrDuringDelegation: Int) throws {
    try unwrap(failBeforeOrDuringDelegation)
    try self.init()
  }

  // CHECK-LABEL: sil hidden @_T021failable_initializers17ThrowDerivedClassCACSi29failBeforeOrDuringDelegation2_tKcfc
  // CHECK: bb0({{.*}}, [[OLD_SELF:%.*]] : $ThrowDerivedClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_BOX]]
  // CHECK:   [[MUI:%.*]] = mark_uninitialized [delegatingself] [[PB_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[MUI]]
  // CHECK:   [[MOVE_ONLY_SELF:%.*]] = load [take] [[MUI]]
  // CHECK:   try_apply {{.*}}({{.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[SUCC_BB1:bb[0-9]+]], error [[ERROR_BB1:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB1]](
  // CHECK:   try_apply {{.*}}({{.*}}, [[MOVE_ONLY_SELF]]) : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error), normal [[SUCC_BB2:bb[0-9]+]], error [[ERROR_BB2:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB2]]([[NEW_SELF:%.*]] : $ThrowDerivedClass):
  // CHECK-NEXT: store [[NEW_SELF]] to [init] [[MUI]]
  // CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[MUI]]
  // CHECK-NEXT: destroy_value [[SELF_BOX]]
  // CHECK-NEXT: return [[RESULT]]
  //
  // CHECK: [[ERROR_BB1]]([[ERROR:%.*]] : $Error):
  // CHECK-NEXT: store [[MOVE_ONLY_SELF]] to [init] [[MUI]]
  // CHECK-NEXT: br [[THROWING_BB:bb[0-9]+]]([[ERROR]]
  //
  // CHECK: [[ERROR_BB2]]([[ERROR:%.*]] : $Error):
  // CHECK-NEXT: br [[THROWING_BB]]([[ERROR]]
  //
  // CHECK: [[THROWING_BB]]([[ERROR:%.*]] : $Error):
  // CHECK-NEXT: destroy_value [[SELF_BOX]]
  // CHECK-NEXT: throw [[ERROR]]
  convenience init(failBeforeOrDuringDelegation2: Int) throws {
    try self.init(failBeforeDelegation: unwrap(failBeforeOrDuringDelegation2))
  }

  convenience init(failAfterDelegation: Int) throws {
    self.init(noFail: ())
    try unwrap(failAfterDelegation)
  }

  // CHECK-LABEL: sil hidden @_T021failable_initializers17ThrowDerivedClassCACSi27failDuringOrAfterDelegation_tKcfc : $@convention(method) (Int, @owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error) {
  // CHECK: bb0({{.*}}, [[OLD_SELF:%.*]] : $ThrowDerivedClass):
  // CHECK:   [[SELF_BOX:%.*]] = alloc_box ${ var ThrowDerivedClass }, let, name "self"
  // CHECK:   [[PB_BOX:%.*]] = project_box [[SELF_BOX]]
  // CHECK:   [[MUI:%.*]] = mark_uninitialized [delegatingself] [[PB_BOX]]
  // CHECK:   store [[OLD_SELF]] to [init] [[MUI]]
  // CHECK:   [[MOVE_ONLY_SELF:%.*]] = load [take] [[MUI]]
  // CHECK:   try_apply {{.*}}([[MOVE_ONLY_SELF]]) : $@convention(method) (@owned ThrowDerivedClass) -> (@owned ThrowDerivedClass, @error Error), normal [[SUCC_BB1:bb[0-9]+]], error [[ERROR_BB1:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB1]]([[NEW_SELF:%.*]] : $ThrowDerivedClass):
  // CHECK-NEXT:   store [[NEW_SELF]] to [init] [[MUI]]
  // CHECK-NEXT:   // function_ref
  // CHECK-NEXT:   function_ref @
  // CHECK-NEXT:   try_apply {{.*}}({{.*}}) : $@convention(thin) (Int) -> (Int, @error Error), normal [[SUCC_BB2:bb[0-9]+]], error [[ERROR_BB2:bb[0-9]+]]
  //
  // CHECK: [[SUCC_BB2]](
  // CHECK-NEXT: [[RESULT:%.*]] = load [copy] [[MUI]]
  // CHECK-NEXT: destroy_value [[SELF_BOX]]
  // CHECK-NEXT: return [[RESULT]]
  //
  // CHECK: [[ERROR_BB1]]([[ERROR:%.*]] : $Error):
  // CHECK-NEXT: br [[THROWING_BB:bb[0-9]+]]([[ERROR]]
  //
  // CHECK: [[ERROR_BB2]]([[ERROR:%.*]] : $Error):
  // CHECK-NEXT: br [[THROWING_BB]]([[ERROR]]
  //
  // CHECK: [[THROWING_BB]]([[ERROR:%.*]] : $Error):
  // CHECK-NEXT: destroy_value [[SELF_BOX]]
  // CHECK-NEXT: throw [[ERROR]]
  convenience init(failDuringOrAfterDelegation: Int) throws {
    try self.init()
    try unwrap(failDuringOrAfterDelegation)
  }

  convenience init(failBeforeOrAfterDelegation: Int) throws {
    try unwrap(failBeforeOrAfterDelegation)
    self.init(noFail: ())
    try unwrap(failBeforeOrAfterDelegation)
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
