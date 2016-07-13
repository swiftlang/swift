// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-id-as-any -emit-silgen %s | FileCheck %s
// REQUIRES: objc_interop

import Foundation

protocol P {}
protocol CP: class {}

// CHECK-LABEL: sil hidden @_TF17objc_bridging_any11passingToId
func passingToId<T: CP, U>(receiver: IdLover,
                           string: String,
                           nsString: NSString,
                           object: AnyObject,
                           classGeneric: T,
                           classExistential: CP,
                           generic: U,
                           existential: P,
                           any: Any) {
  // CHECK: bb0([[SELF:%.*]] : $IdLover, [[STRING:%.*]] : $String, [[NSSTRING:%.*]] : $NSString, [[OBJECT:%.*]] : $AnyObject, [[CLASS_GENERIC:%.*]] : $T, [[CLASS_EXISTENTIAL:%.*]] : $CP, [[GENERIC:%.*]] : $*U, [[EXISTENTIAL:%.*]] : $*P, [[ANY:%.*]] : $*protocol<>

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]]
  // CHECK: [[BRIDGE_STRING:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
  // CHECK: [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[STRING]])
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(string)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $IdLover,
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[NSSTRING]] : $NSString : $NSString, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(nsString)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $IdLover,
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[CLASS_GENERIC]] : $T : $T, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(classGeneric)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $IdLover,
  // CHECK: apply [[METHOD]]([[OBJECT]], [[SELF]])
  receiver.takesId(object)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $IdLover,
  // CHECK: [[OPENED:%.*]] = open_existential_ref [[CLASS_EXISTENTIAL]] : $CP
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[OPENED]]
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(classExistential)

  // These cases perform a universal bridging conversion.

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $IdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $U
  // CHECK-NEXT: copy_addr [[GENERIC]] to [initialization] [[COPY]]
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<U>([[COPY]])
  // CHECK-NEXT: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: strong_release [[ANYOBJECT]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  receiver.takesId(generic)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $IdLover,
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

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF]] : $IdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $protocol<>
  // CHECK-NEXT: copy_addr [[ANY]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr [[COPY]] : $*protocol<> to $*[[OPENED_TYPE:@opened.*protocol<>]],
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK-NEXT: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: strong_release [[ANYOBJECT]]
  // CHECK-NEXT: deinit_existential_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  receiver.takesId(any)

  // TODO: Property and subscript setters
}

// TODO: Look through value-to-optional and optional-to-optional conversions.
/*
func passingToNullableId(receiver: IdLover,
                         string: String,
                         nsString: NSString,
                         object: AnyObject,
                         any: Any,
                         optString: String?,
                         optNSString: NSString?,
                         optObject: AnyObject?,
                         optAny: Any?)
{
  receiver.takesNullableId(string)
  receiver.takesNullableId(nsString)
  receiver.takesNullableId(object)
  receiver.takesNullableId(any)
  receiver.takesNullableId(optString)
  receiver.takesNullableId(optNSString)
  receiver.takesNullableId(optObject)
  receiver.takesNullableId(optAny)
}
 */

// TODO: casting from id, nullable or not

// Make sure we generate correct bridging thunks
class SwiftIdLover : NSObject {

  func methodReturningAny() -> Any {}
  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover18methodReturningAnyfT_P_ : $@convention(method) (@guaranteed SwiftIdLover) -> @out protocol<>
  // CHECK: [[NATIVE_RESULT:%.*]] = alloc_stack $protocol<>
  // CHECK: [[NATIVE_IMP:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover18methodReturningAny
  // CHECK: apply [[NATIVE_IMP]]([[NATIVE_RESULT]], %0)
  // CHECK: [[OPEN_RESULT:%.*]] = open_existential_addr [[NATIVE_RESULT]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK: [[OBJC_RESULT:%.*]] = apply [[BRIDGE_ANYTHING]]<{{.*}}>([[OPEN_RESULT]])
  // CHECK: return [[OBJC_RESULT]]

  func methodTakingAny(a: Any) {}
  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover15methodTakingAnyfT1aP__T_ : $@convention(objc_method) (AnyObject, SwiftIdLover) -> ()
  // CHECK:     bb0(%0 : $AnyObject, %1 : $SwiftIdLover):
  // CHECK-NEXT:  strong_retain %0
  // CHECK-NEXT:  strong_retain %1
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_ref %0
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $protocol<>
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = init_existential_addr [[RESULT]]
  // CHECK-NEXT:  store [[OPENED]] to [[RESULT_VAL]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover15methodTakingAnyfT1aP__T_
  // CHECK-NEXT:  apply [[METHOD]]([[RESULT]], %1)
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  strong_release %1
  // CHECK-NEXT:  return

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover26methodTakingBlockTakingAnyfFP_T_T_ : $@convention(method) (@owned @callee_owned (@in protocol<>) -> (), @guaranteed SwiftIdLover) -> ()

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
  // CHECK:     bb0(%0 : $*protocol<>, %1 : $@convention(block) (AnyObject) -> ()):
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_addr %0 : $*protocol<> to $*[[OPENED_TYPE:@opened.*protocol<>]],
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

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_ : $@convention(method) (@guaranteed SwiftIdLover) -> @owned @callee_owned (@in protocol<>) -> ()

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_ : $@convention(objc_method) (SwiftIdLover) -> @autoreleased @convention(block) (AnyObject) -> ()
  // CHECK:     bb0(%0 : $SwiftIdLover):
  // CHECK-NEXT:  strong_retain %0
  // CHECK:       [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD:%.*]](%0)
  // CHECK-NEXT:  strong_release %0
  // CHECK-NEXT:  [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage @callee_owned (@in protocol<>) -> ()
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  store [[RESULT:%.*]] to [[BLOCK_STORAGE_ADDR]]
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @_TTRXFo_iP___XFdCb_dPs9AnyObject___
  // CHECK-NEXT:  [[BLOCK_HEADER:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_owned (@in protocol<>) -> (), invoke [[THUNK_FN]]
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block [[BLOCK_HEADER]]
  // CHECK-NEXT:  dealloc_stack [[BLOCK_STORAGE]]
  // CHECK-NEXT:  strong_release [[RESULT]]
  // CHECK-NEXT:  return [[BLOCK]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_iP___XFdCb_dPs9AnyObject___ : $@convention(c) @pseudogeneric (@inout_aliasable @block_storage @callee_owned (@in protocol<>) -> (), AnyObject) -> ()
  // CHECK:     bb0(%0 : $*@block_storage @callee_owned (@in protocol<>) -> (), %1 : $AnyObject):
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage %0
  // CHECK-NEXT:  [[FUNCTION:%.*]] = load [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  strong_retain [[FUNCTION]]
  // CHECK-NEXT:  strong_retain %1
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_ref %1
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $protocol<>
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = init_existential_addr [[RESULT]] : $*protocol<>
  // CHECK-NEXT:  store [[OPENED]] to [[RESULT_VAL]]
  // CHECK-NEXT:  apply [[FUNCTION]]([[RESULT]])
  // CHECK-NEXT:  [[VOID:%.*]] = tuple ()
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  return [[VOID]] : $()

  func methodReturningBlockTakingAny() -> ((Any) -> ()) {}

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover29methodTakingBlockReturningAnyfFT_P_T_ : $@convention(method) (@owned @callee_owned () -> @out protocol<>, @guaranteed SwiftIdLover) -> () {

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

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFdCb__aPs9AnyObject__XFo__iP__ : $@convention(thin) (@owned @convention(block) () -> @autoreleased AnyObject) -> @out protocol<>
  // CHECK:     bb0(%0 : $*protocol<>, %1 : $@convention(block) () -> @autoreleased AnyObject):
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply %1()
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_ref [[BRIDGED]]
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $protocol<>
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = init_existential_addr [[RESULT]]
  // CHECK-NEXT:  store [[OPENED]] to [[RESULT_VAL]]

  // TODO: Should elide the copy
  // CHECK-NEXT:  copy_addr [take] [[RESULT]] to [initialization] %0
  // CHECK-NEXT:  [[EMPTY:%.*]] = tuple ()
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  strong_release %1
  // CHECK-NEXT:  return [[EMPTY]]

  func methodTakingBlockReturningAny(_: () -> Any) {}

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_ : $@convention(method) (@guaranteed SwiftIdLover) -> @owned @callee_owned () -> @out protocol<>

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_ : $@convention(objc_method) (SwiftIdLover) -> @autoreleased @convention(block) () -> @autoreleased AnyObject
  // CHECK:     bb0(%0 : $SwiftIdLover):
  // CHECK-NEXT:  strong_retain %0
  // CHECK:       [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_
  // CHECK-NEXT:  [[FUNCTION:%.*]] = apply [[METHOD]](%0)
  // CHECK-NEXT:  strong_release %0
  // CHECK-NEXT:  [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage @callee_owned () -> @out protocol<>
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  store [[FUNCTION]] to [[BLOCK_STORAGE_ADDR]]
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @_TTRXFo__iP__XFdCb__aPs9AnyObject__
  // CHECK-NEXT:  [[BLOCK_HEADER:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_owned () -> @out protocol<>, invoke [[THUNK_FN]]
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block [[BLOCK_HEADER]]
  // CHECK-NEXT:  dealloc_stack [[BLOCK_STORAGE]]
  // CHECK-NEXT:  strong_release [[FUNCTION]]
  // CHECK-NEXT:  return [[BLOCK]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__iP__XFdCb__aPs9AnyObject__ : $@convention(c) @pseudogeneric (@inout_aliasable @block_storage @callee_owned () -> @out protocol<>) -> @autoreleased AnyObject
  // CHECK:     bb0(%0 : $*@block_storage @callee_owned () -> @out protocol<>):
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage %0
  // CHECK-NEXT:  [[FUNCTION:%.*]] = load [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  strong_retain [[FUNCTION]]
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $protocol<>
  // CHECK-NEXT:  apply [[FUNCTION]]([[RESULT]])
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_addr [[RESULT]] : $*protocol<> to $*[[OPENED_TYPE:@opened.*protocol<>]],
  // CHECK-NEXT:  // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT:  [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED]])
  // CHECK-NEXT:  deinit_existential_addr [[RESULT]]
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  return [[BRIDGED]]

  func methodReturningBlockReturningAny() -> (() -> Any) {}
}
