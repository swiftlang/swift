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

protocol Protocol {
  associatedtype AssocType
  static func useInput(_ input: Builtin.Int32, into processInput: (AssocType) -> ())
}

///////////
// Tests //
///////////

class KlassWithBuffer {
  var buffer: Buffer

  // Make sure that the allocating init forwards into the initializing init at +1.
  // CHECK-LABEL: sil hidden @_T0s15KlassWithBufferCABs0A0C3inK_tcfC : $@convention(method) (@owned Klass, @thick KlassWithBuffer.Type) -> @owned KlassWithBuffer {
  // CHECK: bb0([[ARG:%.*]] : @owned $Klass,
  // CHECK:   [[INITIALIZING_INIT:%.*]] = function_ref @_T0s15KlassWithBufferCABs0A0C3inK_tcfc : $@convention(method) (@owned Klass, @owned KlassWithBuffer) -> @owned KlassWithBuffer
  // CHECK:   apply [[INITIALIZING_INIT]]([[ARG]],
  // CHECK: } // end sil function '_T0s15KlassWithBufferCABs0A0C3inK_tcfC'
  init(inK: Klass = Klass()) {
    buffer = Buffer(inK: inK)
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

  // CHECK-LABEL: sil hidden @_T0s28StructContainingBridgeObjectVAByXl8swiftObj_tcfC : $@convention(method) (@owned AnyObject, @thin StructContainingBridgeObject.Type) -> @owned StructContainingBridgeObject {
  // CHECK: bb0([[ARG:%.*]] : @owned $AnyObject,
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   [[CASTED_ARG:%.*]] = unchecked_ref_cast [[BORROWED_ARG]] : $AnyObject to $Builtin.BridgeObject
  // CHECK:   [[COPY_CASTED_ARG:%.*]] = copy_value [[CASTED_ARG]]
  // CHECK:   assign [[COPY_CASTED_ARG]] to
  // CHECK: } // end sil function '_T0s28StructContainingBridgeObjectVAByXl8swiftObj_tcfC'
  init(swiftObj: AnyObject) {
    rawValue = Builtin.reinterpretCast(swiftObj)
  }
}

struct ReabstractionThunkTest : Protocol {
  typealias AssocType = Builtin.Int32

  static func useInput(_ input: Builtin.Int32, into processInput: (AssocType) -> ()) {
    processInput(input)
  }
}
