// RUN: %target-swift-frontend -parse-as-library -module-name Swift -parse-stdlib -emit-silgen -enable-sil-ownership -enable-guaranteed-normal-arguments %s | %FileCheck %s

// This test checks specific codegen related to normal arguments being passed at
// +0. Eventually, it should be merged into normal SILGen tests.

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

class Klass2 {
  var buffer: Buffer
  init() {
    buffer = Buffer(inK: Klass())
  }

  // This test makes sure that we:
  //
  // 1. Are able to propagate a +0 value value buffer.k into a +0 value and that
  // we then copy that +0 value into a +1 value, before we begin the epilog and
  // then return that value.
  // CHECK-LABEL: sil hidden @_T0s6Klass2C23getBufferAsNativeObjectBoyF : $@convention(method) (@guaranteed Klass2) -> @owned Builtin.NativeObject {
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $Klass2):
  // CHECK:   [[BUF_BOX:%.*]] = alloc_stack $Buffer
  // CHECK:   [[METHOD:%.*]] = class_method [[SELF]] : $Klass2, #Klass2.buffer!getter.1
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
  // CHECK: } // end sil function '_T0s6Klass2C23getBufferAsNativeObjectBoyF'
  func getBufferAsNativeObject() -> Builtin.NativeObject {
    return Builtin.unsafeCastToNativeObject(buffer.k)
  }
}

