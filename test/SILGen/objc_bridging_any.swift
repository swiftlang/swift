// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -module-name objc_bridging_any -Xllvm -sil-print-debuginfo -enable-sil-ownership %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation
import objc_generics

protocol P {}
protocol CP: class {}

struct KnownUnbridged {}

// CHECK-LABEL: sil hidden @$s17objc_bridging_any11passingToId{{.*}}F
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
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $NSIdLover,
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

  // CHECK:   [[STRING_COPY:%.*]] = copy_value [[STRING]]
  // CHECK:   [[BRIDGE_STRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_STRING_COPY:%.*]] = begin_borrow [[STRING_COPY]]
  // CHECK:   [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[BORROWED_STRING_COPY]])
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK:   end_borrow [[BORROWED_STRING_COPY]]
  // CHECK:   destroy_value [[STRING_COPY]]
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]]
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  receiver.takesId(string)

  // CHECK:   [[NSSTRING_COPY:%.*]] = copy_value [[NSSTRING]]
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[NSSTRING_COPY]] : $NSString : $NSString, $AnyObject
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  receiver.takesId(nsString)

  // CHECK:   [[CLASS_GENERIC_COPY:%.*]] = copy_value [[CLASS_GENERIC]]
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[CLASS_GENERIC_COPY]] : $T : $T, $AnyObject
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  receiver.takesId(classGeneric)

  // CHECK:   [[OBJECT_COPY:%.*]] = copy_value [[OBJECT]]
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[OBJECT_COPY]], [[SELF]])
  // CHECK:   destroy_value [[OBJECT_COPY]]
  receiver.takesId(object)

  // CHECK:   [[CLASS_EXISTENTIAL_COPY:%.*]] = copy_value [[CLASS_EXISTENTIAL]]
  // CHECK:   [[OPENED:%.*]] = open_existential_ref [[CLASS_EXISTENTIAL_COPY]] : $CP
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[OPENED]]
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  receiver.takesId(classExistential)

  // These cases perform a universal bridging conversion.

  // CHECK:   [[COPY:%.*]] = alloc_stack $U
  // CHECK:   copy_addr [[GENERIC]] to [initialization] [[COPY]]
  // CHECK:   // function_ref _bridgeAnythingToObjectiveC
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<U>([[COPY]])
  // CHECK:   dealloc_stack [[COPY]]
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  receiver.takesId(generic)

  // CHECK:   [[COPY:%.*]] = alloc_stack $P
  // CHECK:   copy_addr [[EXISTENTIAL]] to [initialization] [[COPY]]
  // CHECK:   [[OPENED_COPY:%.*]] = open_existential_addr immutable_access [[COPY]] : $*P to $*[[OPENED_TYPE:@opened.*P]],
  // CHECK:   [[TMP:%.*]] = alloc_stack $[[OPENED_TYPE]]
  // CHECK:   copy_addr [[OPENED_COPY]] to [initialization] [[TMP]]
  // CHECK:   // function_ref _bridgeAnythingToObjectiveC
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[TMP]])
  // CHECK:   dealloc_stack [[TMP]]
  // CHECK:   destroy_addr [[COPY]]
  // CHECK:   dealloc_stack [[COPY]]
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  receiver.takesId(existential)

  // CHECK:   [[ERROR_COPY:%.*]] = copy_value [[ERROR]] : $Error
  // CHECK:   [[ERROR_BOX:%[0-9]+]] = open_existential_box [[ERROR_COPY]] : $Error to $*@opened([[ERROR_ARCHETYPE:"[^"]*"]]) Error
  // CHECK:   [[ERROR_STACK:%[0-9]+]] = alloc_stack $@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK:   copy_addr [[ERROR_BOX]] to [initialization] [[ERROR_STACK]] : $*@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK:   [[BRIDGE_FUNCTION:%[0-9]+]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK:   [[BRIDGED_ERROR:%[0-9]+]] = apply [[BRIDGE_FUNCTION]]<@opened([[ERROR_ARCHETYPE]]) Error>([[ERROR_STACK]])
  // CHECK:   dealloc_stack [[ERROR_STACK]] : $*@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK:   destroy_value [[ERROR_COPY]] : $Error
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[BRIDGED_ERROR]], [[SELF]])
  // CHECK:   destroy_value [[BRIDGED_ERROR]] : $AnyObject
  receiver.takesId(error)

  // CHECK:   [[COPY:%.*]] = alloc_stack $Any
  // CHECK:   copy_addr [[ANY]] to [initialization] [[COPY]]
  // CHECK:   [[OPENED_COPY:%.*]] = open_existential_addr immutable_access [[COPY]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK:   [[TMP:%.*]] = alloc_stack $[[OPENED_TYPE]]
  // CHECK:   copy_addr [[OPENED_COPY]] to [initialization] [[TMP]]
  // CHECK:   // function_ref _bridgeAnythingToObjectiveC
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[TMP]])
  // CHECK:   destroy_addr [[COPY]]
  // CHECK:   dealloc_stack [[COPY]]
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  // CHECK:   destroy_value [[ANYOBJECT]]
  receiver.takesId(any)

  // CHECK:   [[TMP:%.*]] = alloc_stack $KnownUnbridged
  // CHECK:   store [[KNOWN_UNBRIDGED]] to [trivial] [[TMP]]
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveC{{.*}}F
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<KnownUnbridged>([[TMP]])
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(knownUnbridged)

  // These cases bridge using Optional's _ObjectiveCBridgeable conformance.

  // CHECK:   [[OPT_STRING_COPY:%.*]] = copy_value [[OPT_STRING]]
  // CHECK:   [[BRIDGE_OPTIONAL:%.*]] = function_ref @$sSq19_bridgeToObjectiveCyXlyF
  // CHECK:   [[TMP:%.*]] = alloc_stack $Optional<String>
  // CHECK:   [[BORROWED_OPT_STRING_COPY:%.*]] = begin_borrow [[OPT_STRING_COPY]]
  // CHECK:   store_borrow [[BORROWED_OPT_STRING_COPY]] to [[TMP]]
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_OPTIONAL]]<String>([[TMP]])
  // CHECK:   end_borrow [[BORROWED_OPT_STRING_COPY]]
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(optionalA)

  // CHECK:   [[OPT_NSSTRING_COPY:%.*]] = copy_value [[OPT_NSSTRING]]
  // CHECK:   [[BRIDGE_OPTIONAL:%.*]] = function_ref @$sSq19_bridgeToObjectiveCyXlyF
  // CHECK:   [[TMP:%.*]] = alloc_stack $Optional<NSString>
  // CHECK:   [[BORROWED_OPT_NSSTRING_COPY:%.*]] = begin_borrow [[OPT_NSSTRING_COPY]]
  // CHECK:   store_borrow [[BORROWED_OPT_NSSTRING_COPY]] to [[TMP]]
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_OPTIONAL]]<NSString>([[TMP]])
  // CHECK:   end_borrow [[BORROWED_OPT_NSSTRING_COPY]]
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(optionalB)

  // CHECK:   [[TMP:%.*]] = alloc_stack $Optional<Any>
  // CHECK:   copy_addr [[OPT_ANY]] to [initialization] [[TMP]]
  // CHECK:   [[BRIDGE_OPTIONAL:%.*]] = function_ref @$sSq19_bridgeToObjectiveCyXlyF
  // CHECK:   [[ANYOBJECT:%.*]] = apply [[BRIDGE_OPTIONAL]]<Any>([[TMP]])
  // CHECK:   [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK:   apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(optionalC)

  // TODO: Property and subscript setters
}

// Once upon a time, as a workaround for rdar://problem/28318984, we had
// to skip the peephole for types with nontrivial SIL lowerings because we
// didn't correctly form the substitutions for a generic
// _bridgeAnythingToObjectiveC call.  That's not true anymore.
func zim() {}
struct Zang {}
// CHECK-LABEL: sil hidden @$s17objc_bridging_any27typesWithNontrivialLowering8receiverySo9NSIdLoverC_tF
func typesWithNontrivialLowering(receiver: NSIdLover) {
  // CHECK: apply {{.*}}<() -> ()>
  receiver.takesId(zim)
  // CHECK: apply {{.*}}<Zang.Type>
  receiver.takesId(Zang.self)
  // CHECK: apply {{.*}}<(() -> (), Zang.Type)>
  receiver.takesId((zim, Zang.self))
  // CHECK: apply {{%.*}}<(Int, String)>
  receiver.takesId((0, "one"))
}

// CHECK-LABEL: sil hidden @$s17objc_bridging_any19passingToNullableId{{.*}}F
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
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $NSIdLover,
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

  // CHECK: [[STRING_COPY:%.*]] = copy_value [[STRING]]
  // CHECK: [[BRIDGE_STRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK: [[BORROWED_STRING_COPY:%.*]] = begin_borrow [[STRING_COPY]]
  // CHECK: [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[BORROWED_STRING_COPY]])
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: end_borrow [[BORROWED_STRING_COPY]]
  // CHECK: destroy_value [[STRING_COPY]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]]
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  // CHECK: destroy_value [[OPT_ANYOBJECT]]
  receiver.takesNullableId(string)

  // CHECK: [[NSSTRING_COPY:%.*]] = copy_value [[NSSTRING]]
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[NSSTRING_COPY]] : $NSString : $NSString, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(nsString)

  // CHECK: [[OBJECT_COPY:%.*]] = copy_value [[OBJECT]]
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[OBJECT_COPY]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(object)

  // CHECK: [[CLASS_GENERIC_COPY:%.*]] = copy_value [[CLASS_GENERIC]]
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[CLASS_GENERIC_COPY]] : $T : $T, $AnyObject
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(classGeneric)

  // CHECK: [[CLASS_EXISTENTIAL_COPY:%.*]] = copy_value [[CLASS_EXISTENTIAL]]
  // CHECK: [[OPENED:%.*]] = open_existential_ref [[CLASS_EXISTENTIAL_COPY]] : $CP
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[OPENED]]
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  // CHECK: destroy_value [[OPT_ANYOBJECT]]
  receiver.takesNullableId(classExistential)

  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $U
  // CHECK-NEXT: copy_addr [[GENERIC]] to [initialization] [[COPY]]
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<U>([[COPY]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: destroy_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPT_ANYOBJECT]]
  receiver.takesNullableId(generic)

  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $P
  // CHECK-NEXT: copy_addr [[EXISTENTIAL]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr immutable_access [[COPY]] : $*P to $*[[OPENED_TYPE:@opened.*P]],
  // CHECK: [[TMP:%.*]] = alloc_stack $[[OPENED_TYPE]]
  // CHECK: copy_addr [[OPENED_COPY]] to [initialization] [[TMP]]
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[TMP]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: destroy_addr [[TMP]]
  // CHECK-NEXT: dealloc_stack [[TMP]]
  // CHECK-NEXT: destroy_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPT_ANYOBJECT]]
  receiver.takesNullableId(existential)

  // CHECK-NEXT: [[ERROR_COPY:%.*]] = copy_value [[ERROR]] : $Error
  // CHECK-NEXT: [[ERROR_BOX:%[0-9]+]] = open_existential_box [[ERROR_COPY]] : $Error to $*@opened([[ERROR_ARCHETYPE:"[^"]*"]]) Error
  // CHECK-NEXT: [[ERROR_STACK:%[0-9]+]] = alloc_stack $@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK-NEXT: copy_addr [[ERROR_BOX]] to [initialization] [[ERROR_STACK]] : $*@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK: [[BRIDGE_FUNCTION:%[0-9]+]] = function_ref @$ss27_bridgeAnythingToObjectiveCyyXlxlF
  // CHECK-NEXT: [[BRIDGED_ERROR:%[0-9]+]] = apply [[BRIDGE_FUNCTION]]<@opened([[ERROR_ARCHETYPE]]) Error>([[ERROR_STACK]])
  // CHECK-NEXT: [[BRIDGED_ERROR_OPT:%[0-9]+]] = enum $Optional<AnyObject>, #Optional.some!enumelt.1, [[BRIDGED_ERROR]] : $AnyObject
  // CHECK-NEXT: destroy_addr [[ERROR_STACK]]
  // CHECK-NEXT: dealloc_stack [[ERROR_STACK]] : $*@opened([[ERROR_ARCHETYPE]]) Error
  // CHECK-NEXT: destroy_value [[ERROR_COPY]] : $Error
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK-NEXT: apply [[METHOD]]([[BRIDGED_ERROR_OPT]], [[SELF]])
  // CHECK-NEXT: destroy_value [[BRIDGED_ERROR_OPT]]
  receiver.takesNullableId(error)

  // CHECK-NEXT: [[COPY:%.*]] = alloc_stack $Any
  // CHECK-NEXT: copy_addr [[ANY]] to [initialization] [[COPY]]
  // CHECK-NEXT: [[OPENED_COPY:%.*]] = open_existential_addr immutable_access [[COPY]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK: [[TMP:%.*]] = alloc_stack $[[OPENED_TYPE]]
  // CHECK: copy_addr [[OPENED_COPY]] to [initialization] [[TMP]]
  // CHECK-NEXT: // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT: [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[TMP]])
  // CHECK-NEXT: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK-NEXT: destroy_addr [[TMP]]
  // CHECK-NEXT: dealloc_stack [[TMP]]
  // CHECK-NEXT: destroy_addr [[COPY]]
  // CHECK-NEXT: dealloc_stack [[COPY]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK-NEXT: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  // CHECK-NEXT: destroy_value [[OPT_ANYOBJECT]]
  receiver.takesNullableId(any)

  // CHECK: [[TMP:%.*]] = alloc_stack $KnownUnbridged
  // CHECK: store [[KNOWN_UNBRIDGED]] to [trivial] [[TMP]]
  // CHECK: [[BRIDGE_ANYTHING:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveC{{.*}}F
  // CHECK: [[ANYOBJECT:%.*]] = apply [[BRIDGE_ANYTHING]]<KnownUnbridged>([[TMP]])
  // CHECK: [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]] : $NSIdLover,
  // CHECK: apply [[METHOD]]([[OPT_ANYOBJECT]], [[SELF]])
  receiver.takesNullableId(knownUnbridged)

  // CHECK: [[OPT_STRING_COPY:%.*]] = copy_value [[OPT_STRING]]
  // CHECK: switch_enum [[OPT_STRING_COPY]] : $Optional<String>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  //
  // CHECK: [[SOME_BB]]([[STRING_DATA:%.*]] : @owned $String):
  // CHECK:   [[BRIDGE_STRING:%.*]] = function_ref @$sSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // CHECK:   [[BORROWED_STRING_DATA:%.*]] = begin_borrow [[STRING_DATA]]
  // CHECK:   [[BRIDGED:%.*]] = apply [[BRIDGE_STRING]]([[BORROWED_STRING_DATA]])
  // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[BRIDGED]] : $NSString : $NSString, $AnyObject
  // CHECK:   [[OPT_ANYOBJECT:%.*]] = enum {{.*}} [[ANYOBJECT]]
  // CHECK:   end_borrow [[BORROWED_STRING_DATA]]
  // CHECK:   destroy_value [[STRING_DATA]]
  // CHECK:   br [[JOIN:bb.*]]([[OPT_ANYOBJECT]]
  //
  // CHECK: [[NONE_BB]]:
  // CHECK:   [[OPT_NONE:%.*]] = enum $Optional<AnyObject>, #Optional.none!enumelt
  // CHECK:   br [[JOIN]]([[OPT_NONE]]
  //
  // CHECK: [[JOIN]]([[PHI:%.*]] : @owned $Optional<AnyObject>):
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]]
  // CHECK:   apply [[METHOD]]([[PHI]], [[SELF]])
  // CHECK:   destroy_value [[PHI]]
  receiver.takesNullableId(optString)

  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]]
  receiver.takesNullableId(optNSString)

  // CHECK: [[OPT_OBJECT_COPY:%.*]] = copy_value [[OPT_OBJECT]]
  // CHECK: [[METHOD:%.*]] = objc_method [[SELF]]
  // CHECK: apply [[METHOD]]([[OPT_OBJECT_COPY]], [[SELF]])
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

  @objc func methodReturningAny() -> Any { fatalError() }
  // SEMANTIC ARC TODO: This is another case of pattern matching the body of one
  // function in a different function... Just pattern match the unreachable case
  // to preserve behavior. We should check if it is correct.

  // CHECK-LABEL: sil hidden @$s17objc_bridging_any12SwiftIdLoverC18methodReturningAnyypyF : $@convention(method) (@guaranteed SwiftIdLover) -> @out Any
  // CHECK: unreachable
  // CHECK: } // end sil function '$s17objc_bridging_any12SwiftIdLoverC18methodReturningAnyypyF'

  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any12SwiftIdLoverC18methodReturningAnyypyFTo : $@convention(objc_method) (SwiftIdLover) -> @autoreleased AnyObject {
  // CHECK: bb0([[SELF:%[0-9]+]] : @unowned $SwiftIdLover):
  // CHECK:   [[NATIVE_RESULT:%.*]] = alloc_stack $Any
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]] : $SwiftIdLover
  // CHECK:   [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:   [[NATIVE_IMP:%.*]] = function_ref @$s17objc_bridging_any12SwiftIdLoverC18methodReturningAnyypyF
  // CHECK:   apply [[NATIVE_IMP]]([[NATIVE_RESULT]], [[BORROWED_SELF_COPY]])
  // CHECK:   end_borrow [[BORROWED_SELF_COPY]]
  // CHECK:   destroy_value [[SELF_COPY]]
  // CHECK:   [[OPEN_RESULT:%.*]] = open_existential_addr immutable_access [[NATIVE_RESULT]]
	// CHECK:   [[TMP:%.*]] = alloc_stack
  // CHECK:   copy_addr [[OPEN_RESULT]] to [initialization] [[TMP]]
  // CHECK:   [[BRIDGE_ANYTHING:%.*]] = function_ref @$ss27_bridgeAnythingToObjectiveC{{.*}}F
  // CHECK:   [[OBJC_RESULT:%.*]] = apply [[BRIDGE_ANYTHING]]<{{.*}}>([[TMP]])
  // CHECK:   return [[OBJC_RESULT]]
  // CHECK: } // end sil function '$s17objc_bridging_any12SwiftIdLoverC18methodReturningAnyypyFTo'

  @objc func methodReturningOptionalAny() -> Any? { fatalError() }
  // CHECK-LABEL: sil hidden @$s17objc_bridging_any12SwiftIdLoverC26methodReturningOptionalAnyypSgyF
  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any12SwiftIdLoverC26methodReturningOptionalAnyypSgyFTo
  // CHECK:       function_ref @$ss27_bridgeAnythingToObjectiveC{{.*}}F

  @objc func methodTakingAny(a: Any) { fatalError() }
  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any12SwiftIdLoverC15methodTakingAny1ayyp_tFTo : $@convention(objc_method) (AnyObject, SwiftIdLover) -> ()
  // CHECK:     bb0([[ARG:%.*]] : @unowned $AnyObject, [[SELF:%.*]] : @unowned $SwiftIdLover):
  // CHECK:   function_ref [[BRIDGE_ANYOBJECT_TO_ANY:@\$ss018_bridgeAnyObjectToB0yypyXlSgF]] : $@convention(thin) (@guaranteed Optional<AnyObject>) -> @out Any
  // CHECK:  [[METHOD:%.*]] = function_ref @$s17objc_bridging_any12SwiftIdLoverC15methodTakingAny1ayyp_tF
  // CHECK-NEXT:  apply [[METHOD]]([[RESULT:%.*]], [[BORROWED_SELF_COPY:%.*]]) :

  @objc func methodTakingOptionalAny(a: Any?) { fatalError() }
  // CHECK-LABEL: sil hidden @$s17objc_bridging_any12SwiftIdLoverC23methodTakingOptionalAny1ayypSg_tF

  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any12SwiftIdLoverC23methodTakingOptionalAny1ayypSg_tFTo

  // CHECK-LABEL: sil hidden @$s17objc_bridging_any12SwiftIdLoverC017methodTakingBlockH3AnyyyyypXEF : $@convention(method) (@noescape @callee_guaranteed (@in_guaranteed Any) -> (), @guaranteed SwiftIdLover) -> ()

  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any12SwiftIdLoverC017methodTakingBlockH3AnyyyyypXEFTo : $@convention(objc_method) (@convention(block) @noescape (AnyObject) -> (), SwiftIdLover) -> ()
  // CHECK:    bb0([[BLOCK:%.*]] : @unowned $@convention(block) @noescape (AnyObject) -> (), [[SELF:%.*]] : @unowned $SwiftIdLover):
  // CHECK-NEXT:  [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK-NEXT:  [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @$syXlIyBy_ypIegn_TR
  // CHECK-NEXT:  [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]([[BLOCK_COPY]])
  // CHECK-NEXT:  [[THUNK_CVT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[THUNK]]
  // CHECK:       [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @$s17objc_bridging_any12SwiftIdLoverC017methodTakingBlockH3AnyyyyypXEF
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD]]([[THUNK_CVT]], [[BORROWED_SELF_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_SELF_COPY]]
  // CHECK-NEXT:  destroy_value [[THUNK]]
  // CHECK-NEXT:  destroy_value [[SELF_COPY]]
  // CHECK-NEXT:  return [[RESULT]]

  // CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$syXlIyBy_ypIegn_TR
  // CHECK:     bb0([[ANY:%.*]] : @trivial $*Any, [[BLOCK:%.*]] : @guaranteed $@convention(block) @noescape (AnyObject) -> ()):
  // CHECK-NEXT:  [[OPENED_ANY:%.*]] = open_existential_addr immutable_access [[ANY]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
	// CHECK:   [[TMP:%.*]] = alloc_stack
  // CHECK:   copy_addr [[OPENED_ANY]] to [initialization] [[TMP]]
  // CHECK-NEXT:  // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT:  [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[TMP]])
  // CHECK-NEXT:  apply [[BLOCK]]([[BRIDGED]])
  // CHECK-NEXT:  [[VOID:%.*]] = tuple ()
  // CHECK-NEXT:  destroy_value [[BRIDGED]]
  // CHECK-NEXT:  destroy_addr [[TMP]]
  // CHECK-NEXT:  dealloc_stack [[TMP]]
  // CHECK-NEXT:  return [[VOID]]

  @objc func methodTakingBlockTakingAny(_: (Any) -> ()) { fatalError() }

  // CHECK-LABEL: sil hidden @$s17objc_bridging_any12SwiftIdLoverC29methodReturningBlockTakingAnyyypcyF : $@convention(method) (@guaranteed SwiftIdLover) -> @owned @callee_guaranteed (@in_guaranteed Any) -> ()

  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any12SwiftIdLoverC29methodReturningBlockTakingAnyyypcyFTo : $@convention(objc_method) (SwiftIdLover) -> @autoreleased @convention(block) (AnyObject) -> ()
  // CHECK:     bb0([[SELF:%.*]] : @unowned $SwiftIdLover):
  // CHECK-NEXT:  [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK-NEXT:  [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @$s17objc_bridging_any12SwiftIdLoverC29methodReturningBlockTakingAnyyypcyF
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD:%.*]]([[BORROWED_SELF_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_SELF_COPY]]
  // CHECK-NEXT:  destroy_value [[SELF_COPY]]
  // CHECK-NEXT:  [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage @callee_guaranteed (@in_guaranteed Any) -> ()
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  store [[RESULT:%.*]] to [init] [[BLOCK_STORAGE_ADDR]]
  // CHECK:       [[THUNK_FN:%.*]] = function_ref @$sypIegn_yXlIeyBy_TR
  // CHECK-NEXT:  [[BLOCK_HEADER:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_guaranteed (@in_guaranteed Any) -> (), invoke [[THUNK_FN]]
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block [[BLOCK_HEADER]]
  // CHECK-NEXT:  destroy_addr [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  dealloc_stack [[BLOCK_STORAGE]]
  // CHECK-NEXT:  return [[BLOCK]]

  // CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypIegn_yXlIeyBy_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed Any) -> (), AnyObject) -> ()
  // CHECK:     bb0([[BLOCK_STORAGE:%.*]] : @trivial $*@block_storage @callee_guaranteed (@in_guaranteed Any) -> (), [[ANY:%.*]] : @unowned $AnyObject):
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  [[FUNCTION:%.*]] = load [copy] [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  [[ANY_COPY:%.*]] = copy_value [[ANY]]
  // CHECK-NEXT:  [[OPENED_ANY:%.*]] = open_existential_ref [[ANY_COPY]]
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  [[INIT:%.*]] = init_existential_addr [[RESULT]] : $*Any
  // CHECK-NEXT:  store [[OPENED_ANY]] to [init] [[INIT]]
  // CHECK-NEXT:  [[BORROW_FUN:%.*]] =  begin_borrow [[FUNCTION]]
  // CHECK-NEXT:  apply [[BORROW_FUN]]([[RESULT]])
  // CHECK-NEXT:  end_borrow [[BORROW_FUN]]
  // CHECK-NEXT:  [[VOID:%.*]] = tuple ()
  // CHECK-NEXT:  destroy_addr [[RESULT]]
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  destroy_value [[FUNCTION]]
  // CHECK-NEXT:  return [[VOID]] : $()

  @objc func methodTakingBlockTakingOptionalAny(_: (Any?) -> ()) { fatalError() }

  @objc func methodReturningBlockTakingAny() -> ((Any) -> ()) { fatalError() }

  // CHECK-LABEL: sil hidden @$s17objc_bridging_any12SwiftIdLoverC29methodTakingBlockReturningAnyyyypyXEF : $@convention(method) (@noescape @callee_guaranteed () -> @out Any, @guaranteed SwiftIdLover) -> () {

  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any12SwiftIdLoverC29methodTakingBlockReturningAnyyyypyXEFTo : $@convention(objc_method) (@convention(block) @noescape () -> @autoreleased AnyObject, SwiftIdLover) -> ()
  // CHECK:     bb0([[BLOCK:%.*]] : @unowned $@convention(block) @noescape () -> @autoreleased AnyObject, [[ANY:%.*]] : @unowned $SwiftIdLover):
  // CHECK-NEXT:  [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK-NEXT:  [[ANY_COPY:%.*]] = copy_value [[ANY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[THUNK_FN:%.*]] = function_ref @$syXlIyBa_ypIegr_TR
  // CHECK-NEXT:  [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]([[BLOCK_COPY]])
  // CHECK-NEXT:  [[THUNK_CVT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[THUNK]]
  // CHECK-NEXT:  [[BORROWED_ANY_COPY:%.*]] = begin_borrow [[ANY_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @$s17objc_bridging_any12SwiftIdLoverC29methodTakingBlockReturningAnyyyypyXEF
  // CHECK-NEXT:  [[RESULT:%.*]] = apply [[METHOD]]([[THUNK_CVT]], [[BORROWED_ANY_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_ANY_COPY]]
  // CHECK-NEXT:  destroy_value [[THUNK]]
  // CHECK-NEXT:  destroy_value [[ANY_COPY]]
  // CHECK-NEXT:  return [[RESULT]]

  // CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$syXlIyBa_ypIegr_TR : $@convention(thin) (@guaranteed @convention(block) @noescape () -> @autoreleased AnyObject) -> @out Any
  // CHECK:     bb0([[ANY_ADDR:%.*]] : @trivial $*Any, [[BLOCK:%.*]] : @guaranteed $@convention(block) @noescape () -> @autoreleased AnyObject):
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply [[BLOCK]]()
  // CHECK-NEXT:  [[OPTIONAL:%.*]] = unchecked_ref_cast [[BRIDGED]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[BRIDGE_TO_ANY:%.*]] = function_ref [[BRIDGE_TO_ANY_FUNC:@.*]] :
  // CHECK-NEXT:  [[BORROWED_OPTIONAL:%.*]] = begin_borrow [[OPTIONAL]]
  // CHECK-NEXT:  [[RESULT_VAL:%.*]] = apply [[BRIDGE_TO_ANY]]([[ANY_ADDR]], [[BORROWED_OPTIONAL]])
  // CHECK-NEXT:  [[EMPTY:%.*]] = tuple ()
  // CHECK-NEXT:  end_borrow [[BORROWED_OPTIONAL]]
  // CHECK-NEXT:  destroy_value [[OPTIONAL]]
  // CHECK-NEXT:  return [[EMPTY]]

  @objc func methodReturningBlockTakingOptionalAny() -> ((Any?) -> ()) { fatalError() }

  @objc func methodTakingBlockReturningAny(_: () -> Any) { fatalError() }

  // CHECK-LABEL: sil hidden @$s17objc_bridging_any12SwiftIdLoverC020methodReturningBlockH3AnyypycyF : $@convention(method) (@guaranteed SwiftIdLover) -> @owned @callee_guaranteed () -> @out Any

  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any12SwiftIdLoverC020methodReturningBlockH3AnyypycyFTo : $@convention(objc_method) (SwiftIdLover) -> @autoreleased @convention(block) () -> @autoreleased AnyObject
  // CHECK:     bb0([[SELF:%.*]] : @unowned $SwiftIdLover):
  // CHECK-NEXT:  [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK-NEXT:  [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[METHOD:%.*]] = function_ref @$s17objc_bridging_any12SwiftIdLoverC020methodReturningBlockH3AnyypycyF
  // CHECK-NEXT:  [[FUNCTION:%.*]] = apply [[METHOD]]([[BORROWED_SELF_COPY]])
  // CHECK-NEXT:  end_borrow [[BORROWED_SELF_COPY]]
  // CHECK-NEXT:  destroy_value [[SELF_COPY]]
  // CHECK-NEXT:  [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage @callee_guaranteed () -> @out Any
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage [[BLOCK_STORAGE]]
  // CHECK-NEXT:  store [[FUNCTION]] to [init] [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  // function_ref
  // CHECK-NEXT:  [[THUNK_FN:%.*]] = function_ref @$sypIegr_yXlIeyBa_TR
  // CHECK-NEXT:  [[BLOCK_HEADER:%.*]] = init_block_storage_header [[BLOCK_STORAGE]] : $*@block_storage @callee_guaranteed () -> @out Any, invoke [[THUNK_FN]]
  // CHECK-NEXT:  [[BLOCK:%.*]] = copy_block [[BLOCK_HEADER]]
  // CHECK-NEXT:  destroy_addr [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  dealloc_stack [[BLOCK_STORAGE]]
  // CHECK-NEXT:  return [[BLOCK]]

  // CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypIegr_yXlIeyBa_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed () -> @out Any) -> @autoreleased AnyObject
  // CHECK:     bb0(%0 : @trivial $*@block_storage @callee_guaranteed () -> @out Any):
  // CHECK-NEXT:  [[BLOCK_STORAGE_ADDR:%.*]] = project_block_storage %0
  // CHECK-NEXT:  [[FUNCTION:%.*]] = load [copy] [[BLOCK_STORAGE_ADDR]]
  // CHECK-NEXT:  [[RESULT:%.*]] = alloc_stack $Any
  // CHECK-NEXT:  [[BORROW_FUN:%.*]] = begin_borrow [[FUNCTION]]
  // CHECK-NEXT:  apply [[BORROW_FUN]]([[RESULT]])
  // CHECK-NEXT:  end_borrow [[BORROW_FUN]]
  // CHECK-NEXT:  [[OPENED:%.*]] = open_existential_addr immutable_access [[RESULT]] : $*Any to $*[[OPENED_TYPE:@opened.*Any]],
  // CHECK:       [[TMP:%.*]] = alloc_stack $[[OPENED_TYPE]]
  // CHECK:       copy_addr [[OPENED]] to [initialization] [[TMP]]
  // CHECK-NEXT:  // function_ref _bridgeAnythingToObjectiveC
  // CHECK-NEXT:  [[BRIDGE_ANYTHING:%.*]] = function_ref
  // CHECK-NEXT:  [[BRIDGED:%.*]] = apply [[BRIDGE_ANYTHING]]<[[OPENED_TYPE]]>([[TMP]])
  // CHECK-NEXT:  destroy_addr [[TMP]]
  // CHECK-NEXT:  dealloc_stack [[TMP]]
  // CHECK-NEXT:  destroy_addr [[RESULT]]
  // CHECK-NEXT:  dealloc_stack [[RESULT]]
  // CHECK-NEXT:  destroy_value [[FUNCTION]]
  // CHECK-NEXT:  return [[BRIDGED]]

  @objc func methodTakingBlockReturningOptionalAny(_: () -> Any?) { fatalError() }

  @objc func methodReturningBlockReturningAny() -> (() -> Any) { fatalError() }

  @objc func methodReturningBlockReturningOptionalAny() -> (() -> Any?) { fatalError() }
  // CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypSgIegr_yXlSgIeyBa_TR
  // CHECK: function_ref @$ss27_bridgeAnythingToObjectiveC{{.*}}F

  override init() { fatalError() }
  @objc dynamic required convenience init(any: Any) { fatalError() }
  @objc dynamic required convenience init(anyMaybe: Any?) { fatalError() }
  @objc dynamic var anyProperty: Any
  @objc dynamic var maybeAnyProperty: Any?

  subscript(_: IndexForAnySubscript) -> Any { get { fatalError() } set { fatalError() } }

  @objc func methodReturningAnyOrError() throws -> Any { fatalError() }
}

class IndexForAnySubscript { }

func dynamicLookup(x: AnyObject) {
  _ = x.anyProperty
  _ = x[IndexForAnySubscript()]
}

extension GenericClass {
  // CHECK-LABEL: sil hidden @$sSo12GenericClassC17objc_bridging_anyE23pseudogenericAnyErasure1xypx_tF :
  @objc func pseudogenericAnyErasure(x: T) -> Any {
    // CHECK: bb0([[ANY_OUT:%.*]] : @trivial $*Any, [[ARG:%.*]] : @guaranteed $T, [[SELF:%.*]] : @guaranteed $GenericClass<T>
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   [[ANYOBJECT:%.*]] = init_existential_ref [[ARG_COPY]] : $T : $T, $AnyObject
    // CHECK:   [[ANY_BUF:%.*]] = init_existential_addr [[ANY_OUT]] : $*Any, $AnyObject
    // CHECK:   store [[ANYOBJECT]] to [init] [[ANY_BUF]]
    return x
  }
  // CHECK: } // end sil function '$sSo12GenericClassC17objc_bridging_anyE23pseudogenericAnyErasure1xypx_tF'
}

// Make sure AnyHashable erasure marks Hashable conformance as used
class AnyHashableClass : NSObject {
  // CHECK-LABEL: sil hidden @$s17objc_bridging_any16AnyHashableClassC07returnsdE0s0dE0VyF
  // CHECK: [[FN:%.*]] = function_ref @$ss21_convertToAnyHashableys0cD0VxSHRzlF
  // CHECK: apply [[FN]]<GenericOption>({{.*}})
  func returnsAnyHashable() -> AnyHashable {
    return GenericOption.multithreaded
  }
}

// CHECK-LABEL: sil hidden @$s17objc_bridging_any33bridgeOptionalFunctionToAnyObject2fnyXlyycSg_tF : $@convention(thin) (@guaranteed Optional<@callee_guaranteed () -> ()>) -> @owned AnyObject
// CHECK: [[BRIDGE:%.*]] = function_ref @$sSq19_bridgeToObjectiveCyXlyF
// CHECK: [[FN:%.*]] = function_ref @$sIeg_ytIegr_TR
// CHECK: partial_apply [callee_guaranteed] [[FN]]
// CHECK: [[SELF:%.*]] = alloc_stack $Optional<@callee_guaranteed () -> @out ()>
// CHECK: apply [[BRIDGE]]<() -> ()>([[SELF]])
func bridgeOptionalFunctionToAnyObject(fn: (() -> ())?) -> AnyObject {
  return fn as AnyObject
}

// When bridging `id _Nonnull` values incoming from unknown ObjC code,
// turn them into `Any` using a runtime call that defends against the
// possibility they still may be nil.

// CHECK-LABEL: sil hidden @$s17objc_bridging_any22bridgeIncomingAnyValueyypSo9NSIdLoverCF
func bridgeIncomingAnyValue(_ receiver: NSIdLover) -> Any {
  // CHECK: function_ref [[BRIDGE_ANYOBJECT_TO_ANY]]
  return receiver.makesId()
}

class SwiftAnyEnjoyer: NSIdLover, NSIdLoving {
  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any15SwiftAnyEnjoyerC7takesIdyyypFTo
  // CHECK: function_ref [[BRIDGE_ANYOBJECT_TO_ANY]]
  override func takesId(_ x: Any) { }

  // CHECK-LABEL: sil hidden [thunk] @$s17objc_bridging_any15SwiftAnyEnjoyerC7takesId11viaProtocolyyp_tFTo 
  // CHECK: function_ref [[BRIDGE_ANYOBJECT_TO_ANY]]
  func takesId(viaProtocol x: Any) { }
}



// CHECK-LABEL: sil_witness_table shared [serialized] GenericOption: Hashable module objc_generics {
// CHECK-NEXT: base_protocol Equatable: GenericOption: Equatable module objc_generics
// CHECK-NEXT: method #Hashable.hashValue!getter.1: {{.*}} : @$sSo13GenericOptionaSHSCSH9hashValueSivgTW
// CHECK-NEXT: method #Hashable.hash!1: {{.*}} : @$sSo13GenericOptionaSHSCSH4hash4intoys6HasherVz_tFTW
// CHECK-NEXT: method #Hashable._rawHashValue!1: {{.*}} : @$sSo13GenericOptionaSHSCSH13_rawHashValue4seedS2i_tFTW
// CHECK-NEXT: }
