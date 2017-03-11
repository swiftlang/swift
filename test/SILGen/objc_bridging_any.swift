// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | %FileCheck %s
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
                           error: Error,
                           any: Any,
                           knownUnbridged: KnownUnbridged,
                           optionalA: String?,
                           optionalB: NSString?,
                           optionalC: Any?) {
  // CHECK: bb0([[SELF:%.*]] : $NSIdLover,
  // CHECK:   debug_value [[STRING:%.*]] : $String
  // CHECK:   debug_value [[NSSTRING:%.*]] : $NSString
  // CHECK:   debug_value [[OBJECT:%.*]] : $AnyObject
  // CHECK:   debug_value [[CLASS_GENERIC:%.*]] : $T
  // CHECK:   debug_value [[CLASS_EXISTENTIAL:%.*]] : $CP
  // CHECK:   debug_value_addr [[GENERIC:%.*]] : $*U
  // CHECK:   debug_value_addr [[EXISTENTIAL:%.*]] : $*P
  // CHECK:   debug_value [[ERROR:%.*]] : $Error
  // CHECK:   debug_value_addr [[ANY:%.*]] : $*Any
  // CHECK:   debug_value [[KNOWN_UNBRIDGED:%.*]] : $KnownUnbridged
  // CHECK:   debug_value [[OPT_STRING:%.*]] : $Optional<String>
  // CHECK:   debug_value [[OPT_NSSTRING:%.*]] : $Optional<NSString>
  // CHECK:   debug_value_addr [[OPT_ANY:%.*]] : $*Optional<Any>

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]]
  // CHECK:   [[BORROWED_STRING:%.*]] = begin_borrow [[STRING]]
  // CHECK:   [[STRING_COPY:%.*]] = copy_value [[BORROWED_STRING]]
  // CHECK:   [[BRIDGE_STRING:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
  // CHECK:   [[BORROWED_STRING_COPY:%.*]] = begin_borrow [[STRING_COPY]]
  // CHECK:   [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[BORROWED_STRING_COPY]])
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[BRIDGED]]
  // CHECK:   end_borrow [[BORROWED_STRING_COPY]] from [[STRING_COPY]]
  // CHECK:   destroy_value [[STRING_COPY]]
  // CHECK:   end_borrow [[BORROWED_STRING]] from [[STRING]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(string)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[BORROWED_NSSTRING:%.*]] = begin_borrow [[NSSTRING]]
  // CHECK:   [[NSSTRING_COPY:%.*]] = copy_value [[BORROWED_NSSTRING]]
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[NSSTRING_COPY]] : $NSString : $NSString, $AnyObject
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[NSSTRING_COPY]]
  // CHECK:   end_borrow [[BORROWED_NSSTRING]] from [[NSSTRING]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(nsString)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[BORROWED_CLASS_GENERIC:%.*]] = begin_borrow [[CLASS_GENERIC]]
  // CHECK:   [[CLASS_GENERIC_COPY:%.*]] = copy_value [[BORROWED_CLASS_GENERIC]]
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[CLASS_GENERIC_COPY]] : $T : $T, $AnyObject
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[CLASS_GENERIC_COPY]]
  // CHECK:   end_borrow [[BORROWED_CLASS_GENERIC]] from [[CLASS_GENERIC]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(classGeneric)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[BORROWED_OBJECT:%.*]] = begin_borrow [[OBJECT]]
  // CHECK:   [[OBJECT_COPY:%.*]] = copy_value [[BORROWED_OBJECT]]
  // CHECK:   apply [[METHOD]]([[OBJECT_COPY]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[OBJECT_COPY]]
  // CHECK:   end_borrow [[BORROWED_OBJECT]] from [[OBJECT]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(object)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[BORROWED_CLASS_EXISTENTIAL:%.*]] = begin_borrow [[CLASS_EXISTENTIAL]]
  // CHECK:   [[CLASS_EXISTENTIAL_COPY:%.*]] = copy_value [[BORROWED_CLASS_EXISTENTIAL]]
  // CHECK:   [[OPENED:%.*]] = open_existential_ref [[CLASS_EXISTENTIAL_COPY]] : $CP
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[OPENED]]
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[CLASS_EXISTENTIAL_COPY]]
  // CHECK:   end_borrow [[BORROWED_CLASS_EXISTENTIAL]] from [[CLASS_EXISTENTIAL]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(classExistential)

  // These cases perform a universal bridging conversion.

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[COPY:%.*]] = alloc_stack $U
  // CHECK:   copy_addr [[GENERIC]] to [initialization] [[COPY]]
  // CHECK:   // function_ref _bridgeAnythingToObjectiveC
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<U>([[COPY]])
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  // CHECK:   dealloc_stack [[COPY]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(generic)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[COPY:%.*]] = alloc_stack $P
  // CHECK:   copy_addr [[EXISTENTIAL]] to [initialization] [[COPY]]
  // CHECK:   [[OPENED_COPY:%.*]] = open_existential_addr immutable_access [[COPY]] : $*P to $*[[OPENED_TYPE:@opened.*P]],
  // CHECK:   // function_ref _bridgeAnythingToObjectiveC
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  // CHECK:   deinit_existential_addr [[COPY]]
  // CHECK:   dealloc_stack [[COPY]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(existential)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[BORROWED_ERROR:%.*]] = begin_borrow [[ERROR]]
  // CHECK:   [[ERROR_COPY:%.*]] = copy_value [[BORROWED_ERROR]] : $Error
  // CHECK:   [[ERROR_BOX:%[0-9]+]] = open_existential_box [[ERROR_COPY]] : $Error to $*@opened([[ERROR_ARCHETYPE:"[^"]*"]]) Error
  // CHECK:   [[ERROR_STACK:%[0-9]+]] = alloc_stack $@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK:   copy_addr [[ERROR_BOX]] to [initialization] [[ERROR_STACK]] : $*@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK:   [[BRIDGE_FUNCTION:%[0-9]+]] = function_ref @_TFs27_bridgeAnythingToObjectiveCurFxPs9AnyObject_
  // CHECK:   [[BRIDGED_ERROR:%[0-9]+]] = apply [[BRIDGE_FUNCTION]]<@opened([[ERROR_ARCHETYPE]]) Error>([[ERROR_STACK]])
  // CHECK:   apply [[METHOD]]([[BRIDGED_ERROR]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[BRIDGED_ERROR]] : $AnyObject
  // CHECK:   dealloc_stack [[ERROR_STACK]] : $*@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK:   destroy_value [[ERROR_COPY]] : $Error
  // CHECK:   end_borrow [[BORROWED_ERROR]] from [[ERROR]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(error)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[COPY:%.*]] = alloc_stack $Any
  // CHECK:   copy_addr [[ANY]] to [initialization] [[COPY]]
  // CHECK:   [[OPENED_COPY:%.*]] = open_existential_addr immutable_access [[COPY]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK:   // function_ref _bridgeAnythingToObjectiveC
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  // CHECK:   deinit_existential_addr [[COPY]]
  // CHECK:   dealloc_stack [[COPY]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(any)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[TMP:%.*]] = alloc_stack $KnownUnbridged
  // CHECK:   store [[KNOWN_UNBRIDGED]] to [trivial] [[TMP]]
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<KnownUnbridged>([[TMP]])
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(knownUnbridged)

  // These cases bridge using Optional's _ObjectiveCBridgeable conformance.

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[BORROWED_OPT_STRING:%.*]] = begin_borrow [[OPT_STRING]]
  // CHECK:   [[OPT_STRING_COPY:%.*]] = copy_value [[BORROWED_OPT_STRING]]
  // CHECK:   [[BRIDGE_OPTIONAL:%.*]] = function_ref @_TFSq19_bridgeToObjectiveCfT_Ps9AnyObject_
  // CHECK:   [[TMP:%.*]] = alloc_stack $Optional<String>
  // CHECK:   store [[OPT_STRING_COPY]] to [init] [[TMP]]
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_OPTIONAL]]<String>([[TMP]])
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   end_borrow [[BORROWED_OPT_STRING]] from [[OPT_STRING]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(optionalA)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[BORROWED_OPT_NSSTRING:%.*]] = begin_borrow [[OPT_NSSTRING]]
  // CHECK:   [[OPT_NSSTRING_COPY:%.*]] = copy_value [[BORROWED_OPT_NSSTRING]]
  // CHECK:   [[BRIDGE_OPTIONAL:%.*]] = function_ref @_TFSq19_bridgeToObjectiveCfT_Ps9AnyObject_
  // CHECK:   [[TMP:%.*]] = alloc_stack $Optional<NSString>
  // CHECK:   store [[OPT_NSSTRING_COPY]] to [init] [[TMP]]
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_OPTIONAL]]<NSString>([[TMP]])
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   end_borrow [[BORROWED_OPT_NSSTRING]] from [[OPT_NSSTRING]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(optionalB)

  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK:   [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK:   [[TMP:%.*]] = alloc_stack $Optional<Any>
  // CHECK:   copy_addr [[OPT_ANY]] to [initialization] [[TMP]]
  // CHECK:   [[BRIDGE_OPTIONAL:%.*]] = function_ref @_TFSq19_bridgeToObjectiveCfT_Ps9AnyObject_
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_OPTIONAL]]<Any>([[TMP]])
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesId(optionalC)

  // TODO: Property and subscript setters
}

// Workaround for rdar://problem/28318984. Skip the peephole for types with
// nontrivial SIL lowerings because we don't correctly form the substitutions
// for a generic _bridgeAnythingToObjectiveC call.
func zim() {}
struct Zang {}
// CHECK-LABEL: sil hidden @_TF17objc_bridging_any27typesWithNontrivialLoweringFT8receiverCSo9NSIdLover_T_
func typesWithNontrivialLowering(receiver: NSIdLover) {
  // CHECK: init_existential_addr {{%.*}} : $*Any, $() -> ()
  receiver.takesId(zim)
  // CHECK: init_existential_addr {{%.*}} : $*Any, $Zang.Type
  receiver.takesId(Zang.self)
  // CHECK: init_existential_addr {{%.*}} : $*Any, $(() -> (), Zang.Type)
  receiver.takesId((zim, Zang.self))
  // CHECK: apply {{%.*}}<(Int, String)>
  receiver.takesId((0, "one"))
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
                                   error: Error,
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
  // CHECK: [[ERROR:%.*]] : $Error
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

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]]
  // CHECK: [[BORROWED_STRING:%.*]] = begin_borrow [[STRING]]
  // CHECK: [[STRING_COPY:%.*]] = copy_value [[BORROWED_STRING]]
  // CHECK: [[BRIDGE_STRING:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
  // CHECK: [[BORROWED_STRING_COPY:%.*]] = begin_borrow [[STRING_COPY]]
  // CHECK: [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[BORROWED_STRING_COPY]])
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK: destroy_value [[BRIDGED]]
  // CHECK: end_borrow [[BORROWED_STRING_COPY]] from [[STRING_COPY]]
  // CHECK: destroy_value [[STRING_COPY]]
  // CHECK: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(string)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK: [[BORROWED_NSSTRING:%.*]] = begin_borrow [[NSSTRING]]
  // CHECK: [[NSSTRING_COPY:%.*]] = copy_value [[BORROWED_NSSTRING]]
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[NSSTRING_COPY]] : $NSString : $NSString, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK: end_borrow [[BORROWED_NSSTRING]] from [[NSSTRING]]
  // CHECK: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(nsString)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK: [[BORROWED_OBJECT:%.*]] = begin_borrow [[OBJECT]]
  // CHECK: [[OBJECT_COPY:%.*]] = copy_value [[BORROWED_OBJECT]]
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[OBJECT_COPY]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK: end_borrow [[BORROWED_OBJECT]] from [[OBJECT]]
  // CHECK: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(object)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK: [[BORROWED_CLASS_GENERIC:%.*]] = begin_borrow [[CLASS_GENERIC]]
  // CHECK: [[CLASS_GENERIC_COPY:%.*]] = copy_value [[BORROWED_CLASS_GENERIC]]
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[CLASS_GENERIC_COPY]] : $T : $T, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK: end_borrow [[BORROWED_CLASS_GENERIC]] from [[CLASS_GENERIC]]
  // CHECK: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(classGeneric)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK: [[BORROWED_CLASS_EXISTENTIAL:%.*]] = begin_borrow [[CLASS_EXISTENTIAL]]
  // CHECK: [[CLASS_EXISTENTIAL_COPY:%.*]] = copy_value [[BORROWED_CLASS_EXISTENTIAL]]
  // CHECK: [[OPENED:%.*]] = open_existential_ref [[CLASS_EXISTENTIAL_COPY]] : $CP
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[OPENED]]
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK: end_borrow [[BORROWED_CLASS_EXISTENTIAL]] from [[CLASS_EXISTENTIAL]]
  // CHECK: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(classExistential)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $U
  // CHECK-NEXT: copy_addr [[GENERIC]] to [initialization] [[COPY]]
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<U>([[COPY]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK-NEXT: destroy_value [[ANYOBJECT]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  // CHECK-NEXT: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(generic)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $P
  // CHECK-NEXT: copy_addr [[EXISTENTIAL]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr immutable_access [[COPY]] : $*P to $*[[OPENED_TYPE:@opened.*P]],
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK-NEXT: destroy_value [[ANYOBJECT]]
  // CHECK-NEXT: deinit_existential_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  // CHECK-NEXT: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(existential)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK: [[BORROWED_ERROR:%.*]] = begin_borrow [[ERROR]]
  // CHECK-NEXT: [[ERROR_COPY:%.*]] = copy_value [[BORROWED_ERROR]] : $Error
  // CHECK-NEXT: [[ERROR_BOX:%[0-9]+]] = open_existential_box [[ERROR_COPY]] : $Error to $*@opened([[ERROR_ARCHETYPE:"[^"]*"]]) Error
  // CHECK-NEXT: [[ERROR_STACK:%[0-9]+]] = alloc_stack $@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK-NEXT: copy_addr [[ERROR_BOX]] to [initialization] [[ERROR_STACK]] : $*@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK: [[BRIDGE_FUNCTION:%[0-9]+]] = function_ref @_TFs27_bridgeAnythingToObjectiveCurFxPs9AnyObject_
  // CHECK-NEXT: [[BRIDGED_ERROR:%[0-9]+]] = apply [[BRIDGE_FUNCTION]]<@opened([[ERROR_ARCHETYPE]]) Error>([[ERROR_STACK]])
  // CHECK-NEXT: [[BRIDGED_ERROR_OPT:%[0-9]+]] = enum $Optional<AnyObject>, #Optional.some!enumelt.1, [[BRIDGED_ERROR]] : $AnyObject
  // CHECK-NEXT: apply [[METHOD]]([[BRIDGED_ERROR_OPT]], [[BORROWED_SELF]])
  // CHECK-NEXT: destroy_value [[BRIDGED_ERROR]] : $AnyObject
  // CHECK-NEXT: dealloc_stack [[ERROR_STACK]] : $*@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK-NEXT: destroy_value [[ERROR_COPY]] : $Error
  // CHECK-NEXT: end_borrow [[BORROWED_ERROR]] from [[ERROR]]
  // CHECK-NEXT: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(error)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [[ANY]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr immutable_access [[COPY]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_COPY]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK-NEXT: destroy_value [[ANYOBJECT]]
  // CHECK-NEXT: deinit_existential_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  // CHECK-NEXT: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(any)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]] : $NSIdLover,
  // CHECK: [[TMP:%.*]] = alloc_stack $KnownUnbridged
  // CHECK: store [[KNOWN_UNBRIDGED]] to [trivial] [[TMP]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<KnownUnbridged>([[TMP]])
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[BORROWED_SELF]])
  // CHECK: end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(knownUnbridged)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]]
  // CHECK: [[BORROWED_OPT_STRING:%.*]] = begin_borrow [[OPT_STRING]]
  // CHECK: [[OPT_STRING_COPY:%.*]] = copy_value [[BORROWED_OPT_STRING]]
  // CHECK: switch_enum [[OPT_STRING_COPY]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[STRING_DATA:%.*]] : $String):
  // CHECK:   [[BRIDGE_STRING:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
  // CHECK:   [[BORROWED_STRING_DATA:%.*]] = begin_borrow [[STRING_DATA]]
  // CHECK:   [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[BORROWED_STRING_DATA]])
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK:   [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK:   end_borrow [[BORROWED_STRING_DATA]] from [[STRING_DATA]]
  // CHECK:   destroy_value [[STRING_DATA]]
  // CHECK:   br [[JOIN:bb.*]]([[OPT_ANYOBJECT]]
  //
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[OPT_NONE:%.*]] = enum $Optional<AnyObject>, #Optional.none!enumelt
  // CHECK:   br [[JOIN]]([[OPT_NONE]]
  //
  // CHECK: [[JOIN]]([[PHI:%.*]] : $Optional<AnyObject>):
  // CHECK:   apply [[METHOD]]([[PHI]], [[BORROWED_SELF]])
  // CHECK:   destroy_value [[PHI]]
  // CHECK:   end_borrow [[BORROWED_OPT_STRING]] from [[OPT_STRING]]
  // CHECK:   end_borrow [[BORROWED_SELF]] from [[SELF]]
  receiver.takesNullableId(optString)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]]
  receiver.takesNullableId(optNSString)

  // CHECK: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[BORROWED_SELF]]
  // CHECK: [[BORROWED_OPT_OBJECT:%.*]] = begin_borrow [[OPT_OBJECT]]
  // CHECK: [[OPT_OBJECT_COPY:%.*]] = copy_value [[BORROWED_OPT_OBJECT]]
  // CHECK: apply [[METHOD]]([[OPT_OBJECT_COPY]], [[BORROWED_SELF]])
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
  // SEMANTIC ARC TODO: This is another case of pattern matching the body of one
  // function in a different function... Just pattern match the unreachable case
  // to preserve behavior. We should check if it is correct.

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover18methodReturningAnyfT_P_ : $@convention(method) (@guaranteed SwiftIdLover) -> @out Any
  // CHECK: unreachable
  // CHECK: } // end sil function '_TFC17objc_bridging_any12SwiftIdLover18methodReturningAnyfT_P_'

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover18methodReturningAnyfT_P_ : $@convention(objc_method) (SwiftIdLover) -> @autoreleased AnyObject {
  // CHECK: bb0([[SELF:%[0-9]+]] : $SwiftIdLover):
  // CHECK:   [[NATIVE_RESULT:%.*]] = alloc_stack $Any
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $SwiftIdLover
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[NATIVE_IMP:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover18methodReturningAny
  // CHECK:   apply [[NATIVE_IMP]]([[NATIVE_RESULT]], [[BORROWED_SELF_COPY]])
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK:   [[OPEN_RESULT:%.*]] = open_existential_addr immutable_access [[NATIVE_RESULT]]
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref @_TFs27_bridgeAnythingToObjectiveC
  // CHECK:   [[OBJC_RESULT:%.*]] = apply [[BRIDGE_ANYTHING]]<{{.*}}>([[OPEN_RESULT]])
  // CHECK:   return [[OBJC_RESULT]]
  // CHECK: } // end sil function '_TToFC17objc_bridging_any12SwiftIdLover18methodReturningAnyfT_P_'

  func methodReturningOptionalAny() -> Any? {}
  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover26methodReturningOptionalAny
  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover26methodReturningOptionalAny
  // CHECK:       function_ref @_TFs27_bridgeAnythingToObjectiveC

  func methodTakingAny(a: Any) {}
  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover15methodTakingAnyfT1aP__T_ : $@convention(objc_method) (AnyObject, SwiftIdLover) -> ()
  // CHECK:     bb0([[ARG:%.*]] : $AnyObject, [[SELF:%.*]] : $SwiftIdLover):
  // CHECK-NEXT:  [[ARG_COPY:%.*]] = copy_value [[ARG]]
  // CHECK-NEXT:  [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK-NEXT:  [[OPTIONAL_ARG_COPY:%.*]] = unchecked_ref_cast [[ARG_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[BRIDGE_TO_ANY:%.*]] = function_ref [[BRIDGE_TO_ANY_FUNC:@.*]] :
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = apply [[BRIDGE_TO_ANY]]([[RESULT]], [[OPTIONAL_ARG_COPY]])
  // CHECK-NEXT:  [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover15methodTakingAnyfT1aP__T_
  // CHECK-NEXT:  apply [[METHOD]]([[RESULT]], [[BORROWED_SELF_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  destroy_value [[SELF_COPY]]
  // CHECK-NEXT:  return

  func methodTakingOptionalAny(a: Any?) {}
  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover23methodTakingOptionalAny

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover23methodTakingOptionalAny
  // CHECK: function_ref [[BRIDGE_TO_ANY_FUNC]]

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover26methodTakingBlockTakingAnyfFP_T_T_ : $@convention(method) (@owned @callee_owned (@in Any) -> (), @guaranteed SwiftIdLover) -> ()

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover26methodTakingBlockTakingAnyfFP_T_T_ : $@convention(objc_method) (@convention(block) (AnyObject) -> (), SwiftIdLover) -> ()
  // CHECK:    bb0([[BLOCK:%.*]] : $@convention(block) (AnyObject) -> (), [[SELF:%.*]] : $SwiftIdLover):
  // CHECK-NEXT:  [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK-NEXT:  [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @_TTRXFdCb_dPs9AnyObject___XFo_iP___
  // CHECK-NEXT:  [[THUNK:%.*]] = partial_apply [[THUNK_FN]]([[BLOCK_COPY]])
  // CHECK:       [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover26methodTakingBlockTakingAnyfFP_T_T_
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD]]([[THUNK]], [[BORROWED_SELF_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT:  destroy_value [[SELF_COPY]]
  // CHECK-NEXT:  return [[RESULT]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFdCb_dPs9AnyObject___XFo_iP___
  // CHECK:     bb0([[ANY:%.*]] : $*Any, [[BLOCK:%.*]] : $@convention(block) (AnyObject) -> ()):
  // CHECK-NEXT:  [[OPENED_ANY:%.*]] = open_existential_addr immutable_access [[ANY]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK-NEXT:  // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT:  [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[OPENED_ANY]])
  // CHECK-NEXT:  apply [[BLOCK]]([[BRIDGED]])
  // CHECK-NEXT:  [[VOID:%.*]] = tuple ()
  // CHECK-NEXT:  destroy_value [[BLOCK]]
  // CHECK-NEXT:  destroy_value [[BRIDGED]]
  // CHECK-NEXT:  deinit_existential_addr [[ANY]]
  // CHECK-NEXT:  return [[VOID]]

  func methodTakingBlockTakingAny(_: (Any) -> ()) {}

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_ : $@convention(method) (@guaranteed SwiftIdLover) -> @owned @callee_owned (@in Any) -> ()

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_ : $@convention(objc_method) (SwiftIdLover) -> @autoreleased @convention(block) (AnyObject) -> ()
  // CHECK:     bb0([[SELF:%.*]] : $SwiftIdLover):
  // CHECK-NEXT:  [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK-NEXT:  [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover29methodReturningBlockTakingAnyfT_FP_T_
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD:%.*]]([[BORROWED_SELF_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT:  destroy_value [[SELF_COPY]]
  // CHECK-NEXT:  [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage @callee_owned (@in Any) -> ()
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  store [[RESULT:%.*]] to [init] [[BLOCK_STORAGE_ADDR]]
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @_TTRXFo_iP___XFdCb_dPs9AnyObject___
  // CHECK-NEXT:  [[BLOCK_HEADER:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_owned (@in Any) -> (), invoke [[THUNK_FN]]
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block [[BLOCK_HEADER]]
  // CHECK-NEXT:  dealloc_stack [[BLOCK_STORAGE]]
  // CHECK-NEXT:  destroy_value [[RESULT]]
  // CHECK-NEXT:  return [[BLOCK]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_iP___XFdCb_dPs9AnyObject___ : $@convention(c) (@inout_aliasable @block_storage @callee_owned (@in Any) -> (), AnyObject) -> ()
  // CHECK:     bb0([[BLOCK_STORAGE:%.*]] : $*@block_storage @callee_owned (@in Any) -> (), [[ANY:%.*]] : $AnyObject):
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  [[FUNCTION:%.*]] = load [copy] [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  [[ANY_COPY:%.*]] = copy_value [[ANY]]
  // CHECK-NEXT:  [[OPTIONAL:%.*]] = unchecked_ref_cast [[ANY_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[BRIDGE_TO_ANY:%.*]] = function_ref [[BRIDGE_TO_ANY_FUNC:@.*]] :
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = apply [[BRIDGE_TO_ANY]]([[RESULT]], [[OPTIONAL]])
  // CHECK-NEXT:  apply [[FUNCTION]]([[RESULT]])
  // CHECK-NEXT:  [[VOID:%.*]] = tuple ()
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  return [[VOID]] : $()

  func methodTakingBlockTakingOptionalAny(_: (Any?) -> ()) {}

  func methodReturningBlockTakingAny() -> ((Any) -> ()) {}

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover29methodTakingBlockReturningAnyfFT_P_T_ : $@convention(method) (@owned @callee_owned () -> @out Any, @guaranteed SwiftIdLover) -> () {

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover29methodTakingBlockReturningAnyfFT_P_T_ : $@convention(objc_method) (@convention(block) () -> @autoreleased AnyObject, SwiftIdLover) -> ()
  // CHECK:     bb0([[BLOCK:%.*]] : $@convention(block) () -> @autoreleased AnyObject, [[ANY:%.*]] : $SwiftIdLover):
  // CHECK-NEXT:  [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK-NEXT:  [[ANY_COPY:%.*]] = copy_value [[ANY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[THUNK_FN:%.*]] = function_ref @_TTRXFdCb__aPs9AnyObject__XFo__iP__
  // CHECK-NEXT:  [[THUNK:%.*]] = partial_apply [[THUNK_FN]]([[BLOCK_COPY]])
  // CHECK-NEXT:  [[BORROWED_ANY_COPY:%.*]] = begin_borrow [[ANY_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover29methodTakingBlockReturningAnyfFT_P_T_
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD]]([[THUNK]], [[BORROWED_ANY_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_ANY_COPY]] from [[ANY_COPY]]
  // CHECK-NEXT:  destroy_value [[ANY_COPY]]
  // CHECK-NEXT:  return [[RESULT]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFdCb__aPs9AnyObject__XFo__iP__ : $@convention(thin) (@owned @convention(block) () -> @autoreleased AnyObject) -> @out Any
  // CHECK:     bb0([[ANY_ADDR:%.*]] : $*Any, [[BLOCK:%.*]] : $@convention(block) () -> @autoreleased AnyObject):
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply [[BLOCK]]()
  // CHECK-NEXT:  [[OPTIONAL:%.*]] = unchecked_ref_cast [[BRIDGED]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[BRIDGE_TO_ANY:%.*]] = function_ref [[BRIDGE_TO_ANY_FUNC:@.*]] :
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = apply [[BRIDGE_TO_ANY]]([[RESULT]], [[OPTIONAL]])

  // TODO: Should elide the copy
  // CHECK-NEXT:  copy_addr [take] [[RESULT]] to [initialization] [[ANY_ADDR]]
  // CHECK-NEXT:  [[EMPTY:%.*]] = tuple ()
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  destroy_value [[BLOCK]]
  // CHECK-NEXT:  return [[EMPTY]]

  func methodReturningBlockTakingOptionalAny() -> ((Any?) -> ()) {}

  func methodTakingBlockReturningAny(_: () -> Any) {}

  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_ : $@convention(method) (@guaranteed SwiftIdLover) -> @owned @callee_owned () -> @out Any

  // CHECK-LABEL: sil hidden [thunk] @_TToFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_ : $@convention(objc_method) (SwiftIdLover) -> @autoreleased @convention(block) () -> @autoreleased AnyObject
  // CHECK:     bb0([[SELF:%.*]] : $SwiftIdLover):
  // CHECK-NEXT:  [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK-NEXT:  [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @_TFC17objc_bridging_any12SwiftIdLover32methodReturningBlockReturningAnyfT_FT_P_
  // CHECK-NEXT:  [[FUNCTION:%.*]] = apply [[METHOD]]([[BORROWED_SELF_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK-NEXT:  destroy_value [[SELF_COPY]]
  // CHECK-NEXT:  [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage @callee_owned () -> @out Any
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  store [[FUNCTION]] to [init] [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[THUNK_FN:%.*]] = function_ref @_TTRXFo__iP__XFdCb__aPs9AnyObject__
  // CHECK-NEXT:  [[BLOCK_HEADER:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_owned () -> @out Any, invoke [[THUNK_FN]]
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block [[BLOCK_HEADER]]
  // CHECK-NEXT:  dealloc_stack [[BLOCK_STORAGE]]
  // CHECK-NEXT:  destroy_value [[FUNCTION]]
  // CHECK-NEXT:  return [[BLOCK]]

  // CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__iP__XFdCb__aPs9AnyObject__ : $@convention(c) (@inout_aliasable @block_storage @callee_owned () -> @out Any) -> @autoreleased AnyObject
  // CHECK:     bb0(%0 : $*@block_storage @callee_owned () -> @out Any):
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage %0
  // CHECK-NEXT:  [[FUNCTION:%.*]] = load [copy] [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  apply [[FUNCTION]]([[RESULT]])
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_addr immutable_access [[RESULT]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
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
  // CHECK-LABEL: sil hidden @_TFE17objc_bridging_anyCSo12GenericClass23pseudogenericAnyErasurefT1xx_P_ :
  func pseudogenericAnyErasure(x: T) -> Any {
    // CHECK: bb0([[ANY_OUT:%.*]] : $*Any, [[ARG:%.*]] : $T, [[SELF:%.*]] : $GenericClass<T>
    // CHECK:   [[ANY_BUF:%.*]] = init_existential_addr [[ANY_OUT]] : $*Any, $AnyObject
    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
    // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[ARG_COPY]] : $T : $T, $AnyObject
    // CHECK:   store [[ANYOBJECT]] to [init] [[ANY_BUF]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK:   destroy_value [[ARG]]
    return x
  }
  // CHECK: } // end sil function '_TFE17objc_bridging_anyCSo12GenericClass23pseudogenericAnyErasurefT1xx_P_'
}

// Make sure AnyHashable erasure marks Hashable conformance as used
class AnyHashableClass : NSObject {
  // CHECK-LABEL: sil hidden @_TFC17objc_bridging_any16AnyHashableClass18returnsAnyHashablefT_Vs11AnyHashable
  // CHECK: [[FN:%.*]] = function_ref @_swift_convertToAnyHashable
  // CHECK: apply [[FN]]<GenericOption>({{.*}})
  func returnsAnyHashable() -> AnyHashable {
    return GenericOption.multithreaded
  }
}

// CHECK-LABEL: sil_witness_table shared [fragile] GenericOption: Hashable module objc_generics {
// CHECK-NEXT: base_protocol Equatable: GenericOption: Equatable module objc_generics
// CHECK-NEXT: method #Hashable.hashValue!getter.1: {{.*}} : @_TTWVSC13GenericOptions8Hashable13objc_genericsFS0_g9hashValueSi
// CHECK-NEXT: }
