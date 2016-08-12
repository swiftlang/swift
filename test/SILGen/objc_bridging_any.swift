// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-id-as-any -emit-silgen %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation
import objc_generics

protocol P {}
protocol CP: class {}

struct KnownUnbridged {}

// CHECK-LABEL: sil hidden @_TF17objc_bridging_any11passingToId
func passingToId<T: CP, U>(receiver: NSIdLover,
                           string: String,
                           nsString: NSString,
                           object: AnyObject,
                           classGeneric: T,
                           classExistential: CP,
                           generic: U,
                           existential: P,
                           any: Any,
                           knownUnbridged: KnownUnbridged,
                           optionalA: String?,
                           optionalB: NSString?,
                           optionalC: Any?) {
  // CHECK: bb0([[SELF:%.*]] : $NSIdLover,
  // CHECK: [[STRING:%.*]] : $String
  // CHECK: [[NSSTRING:%.*]] : $NSString
  // CHECK: [[OBJECT:%.*]] : $AnyObject
  // CHECK: [[CLASS_GENERIC:%.*]] : $T
  // CHECK: [[CLASS_EXISTENTIAL:%.*]] : $CP
  // CHECK: [[GENERIC:%.*]] : $*U
  // CHECK: [[EXISTENTIAL:%.*]] : $*P
  // CHECK: [[ANY:%.*]] : $*Any
  // CHECK: [[KNOWN_UNBRIDGED:%.*]] : $KnownUnbridged
  // CHECK: [[OPT_STRING:%.*]] : $Optional<String>
  // CHECK: [[OPT_NSSTRING:%.*]] : $Optional<NSString>
  // CHECK: [[OPT_ANY:%.*]] : $*Optional<Any>

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]]
  // CHECK: [[BRIDGE_STRING:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
  // CHECK: [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[STRING]])
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(string)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[NSSTRING]] : $NSString : $NSString, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(nsString)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[CLASS_GENERIC]] : $T : $T, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(classGeneric)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: apply [[METHOD]]([[OBJECT]], [[SELF]])
  receiver.takesId(object)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[OPENED:%.*]] = open_existential_ref [[CLASS_EXISTENTIAL]] : $CP
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[OPENED]]
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(classExistential)

  // These cases perform a universal bridging conversion.

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $U
  // CHECK-NEXT: copy_addr [[GENERIC]] to [initialization] [[COPY]]
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<U>([[COPY]])
  // CHECK-NEXT: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: strong_release [[ANYOBJECT]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  receiver.takesId(generic)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $P
  // CHECK-NEXT: copy_addr [[EXISTENTIAL]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr [[COPY]] : $*P to $*[[OPENED_TYPE:@opened.*P]],
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK-NEXT: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: strong_release [[ANYOBJECT]]
  // CHECK-NEXT: deinit_existential_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  receiver.takesId(existential)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [[ANY]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr [[COPY]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK-NEXT: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: strong_release [[ANYOBJECT]]
  // CHECK-NEXT: deinit_existential_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  receiver.takesId(any)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[TMP:%.*]] = alloc_stack $KnownUnbridged
  // CHECK: store [[KNOWN_UNBRIDGED]] to [[TMP]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<KnownUnbridged>([[TMP]])
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(knownUnbridged)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[TMP:%.*]] = alloc_stack $Optional<String>
  // CHECK: store [[OPT_STRING]] to [[TMP]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<Optional<String>>([[TMP]])
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(optionalA)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[TMP:%.*]] = alloc_stack $Optional<NSString>
  // CHECK: store [[OPT_NSSTRING]] to [[TMP]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<Optional<NSString>>([[TMP]])
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(optionalB)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[TMP:%.*]] = alloc_stack $Optional<Any>
  // CHECK: copy_addr [[OPT_ANY]] to [initialization] [[TMP]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<Optional<Any>>([[TMP]])
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(optionalC)

  // TODO: Property and subscript setters
}

// CHECK-LABEL: sil hidden @_TF17objc_bridging_any19passingToNullableId
func passingToNullableId<T: CP, U>(receiver: NSIdLover,
                                   string: String,
                                   nsString: NSString,
                                   object: AnyObject,
                                   classGeneric: T,
                                   classExistential: CP,
                                   generic: U,
                                   existential: P,
                                   any: Any,
                                   knownUnbridged: KnownUnbridged,
                                   optString: String?,
                                   optNSString: NSString?,
                                   optObject: AnyObject?,
                                   optClassGeneric: T?,
                                   optClassExistential: CP?,
                                   optGeneric: U?,
                                   optExistential: P?,
                                   optAny: Any?,
                                   optKnownUnbridged: KnownUnbridged?,
                                   optOptA: String??,
                                   optOptB: NSString??,
                                   optOptC: Any??)
{
  // CHECK: bb0([[SELF:%.*]] : $NSIdLover,
  // CHECK: [[STRING:%.*]] : $String,
  // CHECK: [[NSSTRING:%.*]] : $NSString
  // CHECK: [[OBJECT:%.*]] : $AnyObject
  // CHECK: [[CLASS_GENERIC:%.*]] : $T
  // CHECK: [[CLASS_EXISTENTIAL:%.*]] : $CP
  // CHECK: [[GENERIC:%.*]] : $*U
  // CHECK: [[EXISTENTIAL:%.*]] : $*P
  // CHECK: [[ANY:%.*]] : $*Any,
  // CHECK: [[KNOWN_UNBRIDGED:%.*]] : $KnownUnbridged,
  // CHECK: [[OPT_STRING:%.*]] : $Optional<String>,
  // CHECK: [[OPT_NSSTRING:%.*]] : $Optional<NSString>
  // CHECK: [[OPT_OBJECT:%.*]] : $Optional<AnyObject>
  // CHECK: [[OPT_CLASS_GENERIC:%.*]] : $Optional<T>
  // CHECK: [[OPT_CLASS_EXISTENTIAL:%.*]] : $Optional<CP>
  // CHECK: [[OPT_GENERIC:%.*]] : $*Optional<U>
  // CHECK: [[OPT_EXISTENTIAL:%.*]] : $*Optional<P>
  // CHECK: [[OPT_ANY:%.*]] : $*Optional<Any>
  // CHECK: [[OPT_KNOWN_UNBRIDGED:%.*]] : $Optional<KnownUnbridged>
  // CHECK: [[OPT_OPT_A:%.*]] : $Optional<Optional<String>>
  // CHECK: [[OPT_OPT_B:%.*]] : $Optional<Optional<NSString>>
  // CHECK: [[OPT_OPT_C:%.*]] : $*Optional<Optional<Any>>

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]]
  // CHECK: [[BRIDGE_STRING:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
  // CHECK: [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[STRING]])
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(string)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[NSSTRING]] : $NSString : $NSString, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(nsString)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[OBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(object)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[CLASS_GENERIC]] : $T : $T, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(classGeneric)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[OPENED:%.*]] = open_existential_ref [[CLASS_EXISTENTIAL]] : $CP
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[OPENED]]
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(classExistential)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $U
  // CHECK-NEXT: copy_addr [[GENERIC]] to [initialization] [[COPY]]
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<U>([[COPY]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: strong_release [[ANYOBJECT]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  receiver.takesNullableId(generic)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $P
  // CHECK-NEXT: copy_addr [[EXISTENTIAL]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr [[COPY]] : $*P to $*[[OPENED_TYPE:@opened.*P]],
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: strong_release [[ANYOBJECT]]
  // CHECK-NEXT: deinit_existential_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  receiver.takesNullableId(existential)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [[ANY]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr [[COPY]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: strong_release [[ANYOBJECT]]
  // CHECK-NEXT: deinit_existential_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  receiver.takesNullableId(any)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $NSIdLover,
  // CHECK: [[TMP:%.*]] = alloc_stack $KnownUnbridged
  // CHECK: store [[KNOWN_UNBRIDGED]] to [[TMP]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<KnownUnbridged>([[TMP]])
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(knownUnbridged)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]]
  // CHECK: select_enum [[OPT_STRING]]
  // CHECK: cond_br
  // CHECK: [[STRING_DATA:%.*]] = unchecked_enum_data [[OPT_STRING]]
  // CHECK: [[BRIDGE_STRING:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
  // CHECK: [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[STRING_DATA]])
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: br [[JOIN:bb.*]]([[OPT_ANYOBJECT]]
  // CHECK: [[JOIN]]([[PHI:%.*]] : $Optional<AnyObject>):
  // CHECK: apply [[METHOD]]([[PHI]], [[SELF]])
  receiver.takesNullableId(optString)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]]
  receiver.takesNullableId(optNSString)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]]
  // CHECK: apply [[METHOD]]([[OPT_OBJECT]], [[SELF]])
  receiver.takesNullableId(optObject)
  receiver.takesNullableId(optClassGeneric)
  receiver.takesNullableId(optClassExistential)
  receiver.takesNullableId(optGeneric)
  receiver.takesNullableId(optExistential)
  receiver.takesNullableId(optAny)
  receiver.takesNullableId(optKnownUnbridged)

  receiver.takesNullableId(optOptA)
  receiver.takesNullableId(optOptB)
  receiver.takesNullableId(optOptC)
}

protocol Anyable {
  init(any: Any)
  init(anyMaybe: Any?)
  var anyProperty: Any { get }
  var maybeAnyProperty: Any? { get }
}

// Make sure we generate correct bridging thunks
class SwiftIdLover : NSObject, Anyable {

  func methodReturningAny() -> Any {}
  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover18methodReturningAnyfT_P_ : $@convention(method) (@guaranteed SwiftIdLover) -> @out Any
  // CHECK: [[NATIVE_RESULT:%.*]] = alloc_stack $Any
  // CHECK: [[NATIVE_IMP:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover18methodReturningAny
  // CHECK: apply [[NATIVE_IMP]]([[NATIVE_RESULT]], %0)
  // CHECK: [[OPEN_RESULT:%.*]] = open_existential_addr [[NATIVE_RESULT]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK: [[OBJC_RESULT:%.*]] = apply [[BRIDGE_ANYTHING]]<{{.*}}>([[OPEN_RESULT]])
  // CHECK: return [[OBJC_RESULT]]

  func methodReturningOptionalAny() -> Any? {}
  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover26methodReturningOptionalAny
  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover26methodReturningOptionalAny
  // CHECK:       function_ref @_TFs27_bridgeAnythingToObjectiveC

  func methodTakingAny(a: Any) {}
  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover15methodTakingAnyfT1aP__T_ : $@convention(objc_method) (AnyObject, SwiftIdLover) -> ()
  // CHECK:     bb0(%0 : $AnyObject, %1 : $SwiftIdLover):
  // CHECK-NEXT:  strong_retain %0
  // CHECK-NEXT:  strong_retain %1
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_ref %0
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = init_existential_addr [[RESULT]]
  // CHECK-NEXT:  store [[OPENED]] to [[RESULT_VAL]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover15methodTakingAnyfT1aP__T_
  // CHECK-NEXT:  apply [[METHOD]]([[RESULT]], %1)
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  strong_release %1
  // CHECK-NEXT:  return

  func methodTakingOptionalAny(a: Any?) {}
  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover23methodTakingOptionalAny

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover23methodTakingOptionalAny
  // CHECK: init_existential_addr %11 : $*Any, $@opened({{.*}}) AnyObject

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover26methodTakingBlockTakingAnyfFP_T_T_ : $@convention(method) (@owned @callee_owned (@in Any) -> (), @guaranteed SwiftIdLover) -> ()

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover26methodTakingBlockTakingAnyfFP_T_T_ : $@convention(objc_method) (@convention(block) (AnyObject) -> (), SwiftIdLover) -> ()
  // CHECK:    bb0(%0 : $@convention(block) (AnyObject) -> (), %1 : $SwiftIdLover):
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block %0
  // CHECK-NEXT:  strong_retain %1
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @_TTRXFdCb_dPs9AnyObject___XFo_iP___
  // CHECK-NEXT:  [[THUNK:%.*]] = partial_apply [[THUNK_FN]]([[BLOCK]])
  // CHECK:       [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover26methodTakingBlockTakingAnyfFP_T_T_
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD]]([[THUNK]], %1)
  // CHECK-NEXT:  strong_release %1
  // CHECK-NEXT:  return [[RESULT]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFdCb_dPs9AnyObject___XFo_iP___
  // CHECK:     bb0(%0 : $*Any, %1 : $@convention(block) (AnyObject) -> ()):
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_addr %0 : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK-NEXT:  // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT:  [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED]])
  // CHECK-NEXT:  apply %1([[BRIDGED]])
  // CHECK-NEXT:  [[VOID:%.*]] = tuple ()
  // CHECK-NEXT:  strong_release %1
  // CHECK-NEXT:  strong_release [[BRIDGED]]
  // CHECK-NEXT:  deinit_existential_addr %0
  // CHECK-NEXT:  return [[VOID]]

  func methodTakingBlockTakingAny(_: (Any) -> ()) {}

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_ : $@convention(method) (@guaranteed SwiftIdLover) -> @owned @callee_owned (@in Any) -> ()

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_ : $@convention(objc_method) (SwiftIdLover) -> @autoreleased @convention(block) (AnyObject) -> ()
  // CHECK:     bb0(%0 : $SwiftIdLover):
  // CHECK-NEXT:  strong_retain %0
  // CHECK:       [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD:%.*]](%0)
  // CHECK-NEXT:  strong_release %0
  // CHECK-NEXT:  [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage @callee_owned (@in Any) -> ()
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  store [[RESULT:%.*]] to [[BLOCK_STORAGE_ADDR]]
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @_TTRXFo_iP___XFdCb_dPs9AnyObject___
  // CHECK-NEXT:  [[BLOCK_HEADER:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_owned (@in Any) -> (), invoke [[THUNK_FN]]
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block [[BLOCK_HEADER]]
  // CHECK-NEXT:  dealloc_stack [[BLOCK_STORAGE]]
  // CHECK-NEXT:  strong_release [[RESULT]]
  // CHECK-NEXT:  return [[BLOCK]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_iP___XFdCb_dPs9AnyObject___ : $@convention(c) @pseudogeneric (@inout_aliasable @block_storage @callee_owned (@in Any) -> (), AnyObject) -> ()
  // CHECK:     bb0(%0 : $*@block_storage @callee_owned (@in Any) -> (), %1 : $AnyObject):
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage %0
  // CHECK-NEXT:  [[FUNCTION:%.*]] = load [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  strong_retain [[FUNCTION]]
  // CHECK-NEXT:  strong_retain %1
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_ref %1
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = init_existential_addr [[RESULT]] : $*Any
  // CHECK-NEXT:  store [[OPENED]] to [[RESULT_VAL]]
  // CHECK-NEXT:  apply [[FUNCTION]]([[RESULT]])
  // CHECK-NEXT:  [[VOID:%.*]] = tuple ()
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  return [[VOID]] : $()

  func methodTakingBlockTakingOptionalAny(_: (Any?) -> ()) {}

  func methodReturningBlockTakingAny() -> ((Any) -> ()) {}

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover29methodTakingBlockReturningAnyfFT_P_T_ : $@convention(method) (@owned @callee_owned () -> @out Any, @guaranteed SwiftIdLover) -> () {

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover29methodTakingBlockReturningAnyfFT_P_T_ : $@convention(objc_method) (@convention(block) () -> @autoreleased AnyObject, SwiftIdLover) -> ()
  // CHECK:     bb0(%0 : $@convention(block) () -> @autoreleased AnyObject, %1 : $SwiftIdLover):
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block %0
  // CHECK-NEXT:  strong_retain %1
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @_TTRXFdCb__aPs9AnyObject__XFo__iP__
  // CHECK-NEXT:  [[THUNK:%.*]] = partial_apply [[THUNK_FN]]([[BLOCK]])
  // CHECK:       [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover29methodTakingBlockReturningAnyfFT_P_T_
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD]]([[THUNK]], %1)
  // CHECK-NEXT:  strong_release %1
  // CHECK-NEXT:  return [[RESULT]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFdCb__aPs9AnyObject__XFo__iP__ : $@convention(thin) (@owned @convention(block) () -> @autoreleased AnyObject) -> @out Any
  // CHECK:     bb0(%0 : $*Any, %1 : $@convention(block) () -> @autoreleased AnyObject):
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply %1()
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_ref [[BRIDGED]]
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = init_existential_addr [[RESULT]]
  // CHECK-NEXT:  store [[OPENED]] to [[RESULT_VAL]]

  // TODO: Should elide the copy
  // CHECK-NEXT:  copy_addr [take] [[RESULT]] to [initialization] %0
  // CHECK-NEXT:  [[EMPTY:%.*]] = tuple ()
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  strong_release %1
  // CHECK-NEXT:  return [[EMPTY]]

  func methodReturningBlockTakingOptionalAny() -> ((Any?) -> ()) {}

  func methodTakingBlockReturningAny(_: () -> Any) {}

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_ : $@convention(method) (@guaranteed SwiftIdLover) -> @owned @callee_owned () -> @out Any

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_ : $@convention(objc_method) (SwiftIdLover) -> @autoreleased @convention(block) () -> @autoreleased AnyObject
  // CHECK:     bb0(%0 : $SwiftIdLover):
  // CHECK-NEXT:  strong_retain %0
  // CHECK:       [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_
  // CHECK-NEXT:  [[FUNCTION:%.*]] = apply [[METHOD]](%0)
  // CHECK-NEXT:  strong_release %0
  // CHECK-NEXT:  [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage @callee_owned () -> @out Any
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  store [[FUNCTION]] to [[BLOCK_STORAGE_ADDR]]
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @_TTRXFo__iP__XFdCb__aPs9AnyObject__
  // CHECK-NEXT:  [[BLOCK_HEADER:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_owned () -> @out Any, invoke [[THUNK_FN]]
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block [[BLOCK_HEADER]]
  // CHECK-NEXT:  dealloc_stack [[BLOCK_STORAGE]]
  // CHECK-NEXT:  strong_release [[FUNCTION]]
  // CHECK-NEXT:  return [[BLOCK]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__iP__XFdCb__aPs9AnyObject__ : $@convention(c) @pseudogeneric (@inout_aliasable @block_storage @callee_owned () -> @out Any) -> @autoreleased AnyObject
  // CHECK:     bb0(%0 : $*@block_storage @callee_owned () -> @out Any):
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage %0
  // CHECK-NEXT:  [[FUNCTION:%.*]] = load [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  strong_retain [[FUNCTION]]
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  apply [[FUNCTION]]([[RESULT]])
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_addr [[RESULT]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK-NEXT:  // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT:  [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED]])
  // CHECK-NEXT:  deinit_existential_addr [[RESULT]]
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  return [[BRIDGED]]

  func methodTakingBlockReturningOptionalAny(_: () -> Any?) {}

  func methodReturningBlockReturningAny() -> (() -> Any) {}

  func methodReturningBlockReturningOptionalAny() -> (() -> Any?) {}
  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__iGSqP___XFdCb__aGSqPs9AnyObject___
  // CHECK: function_ref @_TFs27_bridgeAnythingToObjectiveC

  override init() { super.init() }
  dynamic required convenience init(any: Any) { self.init() }
  dynamic required convenience init(anyMaybe: Any?) { self.init() }
  dynamic var anyProperty: Any
  dynamic var maybeAnyProperty: Any?

  subscript(_: IndexForAnySubscript) -> Any { get {} set {} }

  func methodReturningAnyOrError() throws -> Any {}
}

class IndexForAnySubscript {}

func dynamicLookup(x: AnyObject) {
  _ = x.anyProperty
  _ = x[IndexForAnySubscript()]
}

extension GenericClass {
  // CHECK-LABEL: sil hidden @_TFE17objc_bridging_anyCSo12GenericClass23pseudogenericAnyErasurefT1xx_P_
  func pseudogenericAnyErasure(x: T) -> Any {
    // CHECK: [[ANY_BUF:%.*]] = init_existential_addr %0 : $*Any, $AnyObject
    // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref %1 : $T : $T, $AnyObject
    // CHECK: store [[ANYOBJECT]] to [[ANY_BUF]]
    return x
  }
}
