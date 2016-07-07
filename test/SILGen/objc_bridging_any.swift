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
  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF:%.*]] : $IdLover,
  // CHECK: [[BRIDGE_STRING:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveC
  // CHECK: [[NSSTRING:%.*]] = apply [[BRIDGE_STRING]]
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref [[NSSTRING]] : $NSString : $NSString, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(string)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF:%.*]] : $IdLover,
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref {{%.*}} : $NSString : $NSString, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(nsString)

  // CHECK: [[METHOD:%.*]] = class_method [volatile] [[SELF:%.*]] : $IdLover,
  // CHECK: [[ANYOBJECT:%.*]] = init_existential_ref {{%.*}} : $T : $T, $AnyObject
  // CHECK: apply [[METHOD]]([[ANYOBJECT]], [[SELF]])
  receiver.takesId(classGeneric)

  // TODO: Need to look through an (open_existential (erasure)) combo to upcast
  // an existential to Any.
  /*
  receiver.takesId(object)
  receiver.takesId(classExistential)
   */

  // TODO: These cases need to perform a (to-be-implemented) universal
  // bridging conversion.
  /*
  receiver.takesId(generic)
  receiver.takesId(existential)
  receiver.takesId(any)
   */

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
