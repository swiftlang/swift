// RUN: %target-swift-frontend -parse-as-library -module-name Swift -parse-stdlib -emit-silgen -enable-sil-ownership -enable-guaranteed-normal-arguments %s | %FileCheck %s

// This test checks specific codegen related to normal arguments being passed at
// +0. Eventually, it should be merged into normal SILGen tests.

/////////////////
// Fake Stdlib //
/////////////////

precedencegroup AssignmentPrecedence {
  assignment: true
}

enum Optional<T> {
case none
case some(T)
}

class Klass {
  init() {}
}

struct Buffer {
  var k: Klass
  init(inK: Klass) {
    k = inK
  }
}

typealias AnyObject = Builtin.AnyObject

///////////
// Tests //
///////////

class KlassWithBuffer {
  var buffer: Buffer
  init() {
    buffer = Buffer(inK: Klass())
  }

  // This test makes sure that we:
  //
  // 1. Are able to propagate a +0 value value buffer.k into a +0 value and that
  // we then copy that +0 value into a +1 value, before we begin the epilog and
  // then return that value.
  // CHECK-LABEL: sil hidden @_T0s15KlassWithBufferC03getC14AsNativeObjectBoyF : $@convention(method) (@guaranteed KlassWithBuffer) -> @owned Builtin.NativeObject {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $KlassWithBuffer):
  // CHECK:   [[BUF_BOX:%.*]] = alloc_stack $Buffer
  // CHECK:   [[METHOD:%.*]] = class_method [[SELF]] : $KlassWithBuffer, #KlassWithBuffer.buffer!getter.1
  // CHECK:   [[BUF:%.*]] = apply [[METHOD]]([[SELF]])
  // CHECK:   store [[BUF]] to [init] [[BUF_BOX]]
  // CHECK:   [[GEP:%.*]] = struct_element_addr [[BUF_BOX]] : $*Buffer, #Buffer.k
  // CHECK:   [[BUF_KLASS:%.*]] = load [copy] [[GEP]]
  // CHECK:   destroy_addr [[BUF_BOX]]
  // CHECK:   [[BORROWED_BUF_KLASS:%.*]] = begin_borrow [[BUF_KLASS]]
  // CHECK:   [[CASTED_BORROWED_BUF_KLASS:%.*]] = unchecked_ref_cast [[BORROWED_BUF_KLASS]]
  // CHECK:   [[COPY_CASTED_BORROWED_BUF_KLASS:%.*]] = copy_value [[CASTED_BORROWED_BUF_KLASS]]
  // CHECK:   end_borrow [[BORROWED_BUF_KLASS]]
  // CHECK:   destroy_value [[BUF_KLASS]]
  // CHECK:   return [[COPY_CASTED_BORROWED_BUF_KLASS]]
  // CHECK: } // end sil function '_T0s15KlassWithBufferC03getC14AsNativeObjectBoyF'
  func getBufferAsNativeObject() -> Builtin.NativeObject {
    return Builtin.unsafeCastToNativeObject(buffer.k)
  }
}

struct StructContainingBridgeObject {
  var rawValue: Builtin.BridgeObject

  // CHECK-LABEL: sil hidden @_T0s28StructContainingBridgeObjectVAByXl8swiftObj_tcfC : $@convention(method) (@guaranteed AnyObject, @thin StructContainingBridgeObject.Type) -> @owned StructContainingBridgeObject {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $AnyObject,
  // CHECK:   [[CASTED_ARG:%.*]] = unchecked_ref_cast [[ARG]] : $AnyObject to $Builtin.BridgeObject
  // CHECK:   [[COPY_CASTED_ARG:%.*]] = copy_value [[CASTED_ARG]]
  // CHECK:   assign [[COPY_CASTED_ARG]] to
  // CHECK: } // end sil function '_T0s28StructContainingBridgeObjectVAByXl8swiftObj_tcfC'
  init(swiftObj: AnyObject) {
    rawValue = Builtin.reinterpretCast(swiftObj)
  }
}
