// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -module-name objc_sending -Xllvm -sil-print-types -import-objc-header %S/Inputs/objc_sending.h %s | %FileCheck %s
// RUN: %target-swift-emit-sil(mock-sdk: %clang-importer-sdk) -sil-verify-all -module-name objc_sending -Xllvm -sil-print-types -import-objc-header %S/Inputs/objc_sending.h %s

// REQUIRES: objc_interop

import Foundation

// ============================================================================
// Swift method with `sending` witnessing an imported ObjC protocol requirement.

public class Handler: NSObject, ObjCHandlerWithSending {
  public func handle(_ value: sending NSObject) {}

  // CHECK-LABEL: sil {{.*}}[ossa] @$s12objc_sending7HandlerC6handleyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Handler) -> ()
  // CHECK: bb0(%0 : @owned $NSObject, %1 : @guaranteed $Handler):

  // The @objc thunk must receive the sending param as @unowned and retain it:

  // CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending7HandlerC6handleyySo8NSObjectCnFTo : $@convention(objc_method) (@sil_sending NSObject, Handler) -> () {
  // CHECK: bb0([[SENDING_ARG:%.*]] : @unowned $NSObject, [[SELF_ARG:%.*]] : @unowned $Handler):
  // CHECK-NEXT:   [[SENDING_COPY:%.*]] = copy_value [[SENDING_ARG]] : $NSObject
  // CHECK-NEXT:   [[SELF_COPY:%.*]] = copy_value [[SELF_ARG]] : $Handler
  // CHECK-NEXT:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]] : $Handler
  // CHECK:        [[NATIVE_FN:%.*]] = function_ref @$s12objc_sending7HandlerC6handleyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Handler) -> ()
  // CHECK-NEXT:   apply [[NATIVE_FN]]([[SENDING_COPY]], [[SELF_BORROW]]) : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Handler) -> ()
  // CHECK-NEXT:   end_borrow [[SELF_BORROW]] : $Handler
  // CHECK-NEXT:   destroy_value [[SELF_COPY]] : $Handler
  // CHECK:      } // end sil function

  public func identity(_ value: sending NSObject) -> sending NSObject { return value }

  // CHECK-LABEL: sil {{.*}}[ossa] @$s12objc_sending7HandlerC8identityySo8NSObjectCAFnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Handler) -> @sil_sending @owned NSObject {
  // CHECK:         bb0(%0 : @owned $NSObject, %1 : @guaranteed $Handler):
  // CHECK:      } // end sil function

  // CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending7HandlerC8identityySo8NSObjectCAFnFTo : $@convention(objc_method) (@sil_sending NSObject, Handler) -> @sil_sending @autoreleased NSObject {
  // CHECK:         bb0(%0 : @unowned $NSObject, %1 : @unowned $Handler):
  // CHECK:      } // end sil function
}




// =========================================================================================
// Pure Swift @objc method with `sending` this should exercise ObjCSelectorFamilyConventions

public class Processor: NSObject {
  @objc public func process(_ value: sending NSObject) {}

  // The native Swift entry point expects @sil_sending @owned.
  // CHECK-LABEL: sil {{.*}}[ossa] @$s12objc_sending9ProcessorC7processyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Processor) -> ()
  // CHECK: bb0(%0 : @owned $NSObject, %1 : @guaranteed $Processor):
  // CHECK:      } // end sil function


  // CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending9ProcessorC7processyySo8NSObjectCnFTo : $@convention(objc_method) (@sil_sending NSObject, Processor) -> () {
  // CHECK: bb0([[SENDING_ARG:%.*]] : @unowned $NSObject, [[SELF_ARG:%.*]] : @unowned $Processor):
  // CHECK-NEXT:   [[SENDING_COPY:%.*]] = copy_value [[SENDING_ARG]] : $NSObject
  // CHECK-NEXT:   [[SELF_COPY:%.*]] = copy_value [[SELF_ARG]] : $Processor
  // CHECK-NEXT:   [[SELF_BORROW:%.*]] = begin_borrow [[SELF_COPY]] : $Processor
  // CHECK:        [[NATIVE_FN:%.*]] = function_ref @$s12objc_sending9ProcessorC7processyySo8NSObjectCnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Processor) -> ()
  // CHECK-NEXT:   apply [[NATIVE_FN]]([[SENDING_COPY]], [[SELF_BORROW]]) : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Processor) -> ()
  // CHECK-NEXT:   end_borrow [[SELF_BORROW]] : $Processor
  // CHECK-NEXT:   destroy_value [[SELF_COPY]] : $Processor
  // CHECK:      } // end sil function

  @objc public func identity(_ value: sending NSObject) -> sending NSObject { return value }

  // CHECK-LABEL: sil {{.*}}[ossa] @$s12objc_sending9ProcessorC8identityySo8NSObjectCAFnF : $@convention(method) (@sil_sending @owned NSObject, @guaranteed Processor) -> @sil_sending @owned NSObject {
  // CHECK:         bb0(%0 : @owned $NSObject, %1 : @guaranteed $Processor):
  // CHECK:      } // end sil function

  // CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending9ProcessorC8identityySo8NSObjectCAFnFTo : $@convention(objc_method) (@sil_sending NSObject, Processor) -> @sil_sending @autoreleased NSObject {
  // CHECK:         bb0(%0 : @unowned $NSObject, %1 : @unowned $Processor):
  // CHECK:      } // end sil function
}




@objc class SendingTest: NSObject {
    @objc static func foo(_ object: sending Any) {}
    // CHECK-LABEL: sil {{.*}}[ossa] @$s12objc_sending11SendingTestC3fooyyypnFZ : $@convention(method) (@sil_sending @in Any, @thick SendingTest.Type) -> () {
    // CHECK:         bb0(%0 : $*Any, %1 : $@thick SendingTest.Type):
    // CHECK:       } // end sil function

    // CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending11SendingTestC3fooyyypnFZTo : $@convention(objc_method) (@sil_sending AnyObject, @objc_metatype SendingTest.Type) -> () {
    // CHECK:         bb0([[SENDING_ARG:%.*]] : @unowned $AnyObject, %1 : $@objc_metatype SendingTest.Type):
    // CHECK:         [[SENDING_COPY:%.*]] = copy_value [[SENDING_ARG]] : $AnyObject
    // CHECK:         unchecked_ref_cast [[SENDING_COPY]] : $AnyObject to $Optional<AnyObject>
    // CHECK:         function_ref @$s12objc_sending11SendingTestC3fooyyypnFZ : $@convention(method) (@sil_sending @in Any, @thick SendingTest.Type) -> ()
    // CHECK:       } // end sil function

    @objc static func bar(_ object: sending NSObject) -> sending NSObject { return object }
    // CHECK-LABEL: sil {{.*}}[ossa] @$s12objc_sending11SendingTestC3barySo8NSObjectCAFnFZ : $@convention(method) (@sil_sending @owned NSObject, @thick SendingTest.Type) -> @sil_sending @owned NSObject {
    // CHECK: bb0(%0 : @owned $NSObject, %1 : $@thick SendingTest.Type):
    // CHECK:      } // end sil function

    // CHECK-LABEL: sil private [thunk] [ossa] @$s12objc_sending11SendingTestC3barySo8NSObjectCAFnFZTo : $@convention(objc_method) (@sil_sending NSObject, @objc_metatype SendingTest.Type) -> @sil_sending @autoreleased NSObject {
    // CHECK:        bb0([[SENDING_ARG:%.*]] : @unowned $NSObject, %1 : $@objc_metatype SendingTest.Type):
    // CHECK:        [[SENDING_COPY:%.*]] = copy_value [[SENDING_ARG]] : $NSObject
    // CHECK:        [[NATIVE_FN:%.*]] = function_ref @$s12objc_sending11SendingTestC3barySo8NSObjectCAFnFZ : $@convention(method) (@sil_sending @owned NSObject, @thick SendingTest.Type) -> @sil_sending @owned NSObject
    // CHECK:        [[RETVAL:%.*]] = apply [[NATIVE_FN]]([[SENDING_COPY]], {{.*}}) : $@convention(method) (@sil_sending @owned NSObject, @thick SendingTest.Type) -> @sil_sending @owned NSObject
    // CHECK:        return [[RETVAL]] : $NSObject
    // CHECK:      } // end sil function
}
