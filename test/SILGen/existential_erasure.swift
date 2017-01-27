// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

protocol P {
  func downgrade(_ m68k: Bool) -> Self
  func upgrade() throws -> Self
}
protocol Q {}

struct X: P, Q {
  func downgrade(_ m68k: Bool) -> X {
    return self
  }

  func upgrade() throws -> X {
    return self
  }
}

func makePQ() -> P & Q { return X() }

func useP(_ x: P) { }

func throwingFunc() throws -> Bool { return true }

// CHECK-LABEL: sil hidden @_T019existential_erasure5PQtoPyyF : $@convention(thin) () -> () {
func PQtoP() {
  // CHECK: [[PQ_PAYLOAD:%.*]] = open_existential_addr immutable_access [[PQ:%.*]] : $*P & Q to $*[[OPENED_TYPE:@opened(.*) P & Q]]
  // CHECK: [[P_PAYLOAD:%.*]] = init_existential_addr [[P:%.*]] : $*P, $[[OPENED_TYPE]]
  // CHECK: copy_addr [take] [[PQ_PAYLOAD]] to [initialization] [[P_PAYLOAD]]
  // CHECK: deinit_existential_addr [[PQ]]
  // CHECK-NOT: destroy_addr [[P]]
  // CHECK-NOT: destroy_addr [[P_PAYLOAD]]
  // CHECK-NOT: destroy_addr [[PQ]]
  // CHECK-NOT: destroy_addr [[PQ_PAYLOAD]]
  useP(makePQ())
}

// Make sure uninitialized existentials are properly deallocated when we
// have an early return.

// CHECK-LABEL: sil hidden @_T019existential_erasure19openExistentialToP1yAA1P_pKF
func openExistentialToP1(_ p: P) throws {
// CHECK: bb0(%0 : $*P):
// CHECK:   [[OPEN:%.*]] = open_existential_addr immutable_access %0 : $*P to $*[[OPEN_TYPE:@opened\(.*\) P]]
// CHECK:   [[RESULT:%.*]] = alloc_stack $P
// CHECK:   [[RESULT_ADDR:%.*]] = init_existential_addr [[RESULT]] : $*P, $[[OPEN_TYPE]]
// CHECK:   [[METHOD:%.*]] = witness_method $[[OPEN_TYPE]], #P.downgrade!1 : {{.*}}, [[OPEN]]
// CHECK:   [[FUNC:%.*]] = function_ref @_T019existential_erasure12throwingFuncSbyKF
// CHECK:   try_apply [[FUNC]]()
//
// CHECK: bb1([[SUCCESS:%.*]] : $Bool):
// CHECK:   apply [[METHOD]]<[[OPEN_TYPE]]>([[RESULT_ADDR]], [[SUCCESS]], [[OPEN]])
// CHECK:   dealloc_stack [[RESULT]]
// CHECK:   destroy_addr %0
// CHECK:   return
//
// CHECK: bb2([[FAILURE:%.*]] : $Error):
// CHECK:   deinit_existential_addr [[RESULT]]
// CHECK:   dealloc_stack [[RESULT]]
// CHECK:   destroy_addr %0
// CHECK:   throw [[FAILURE]]
//
  try useP(p.downgrade(throwingFunc()))
}

// CHECK-LABEL: sil hidden @_T019existential_erasure19openExistentialToP2yAA1P_pKF
func openExistentialToP2(_ p: P) throws {
// CHECK: bb0(%0 : $*P):
// CHECK:   [[OPEN:%.*]] = open_existential_addr immutable_access %0 : $*P to $*[[OPEN_TYPE:@opened\(.*\) P]]
// CHECK:   [[RESULT:%.*]] = alloc_stack $P
// CHECK:   [[RESULT_ADDR:%.*]] = init_existential_addr [[RESULT]] : $*P, $[[OPEN_TYPE]]
// CHECK:   [[METHOD:%.*]] = witness_method $[[OPEN_TYPE]], #P.upgrade!1 : {{.*}}, [[OPEN]]
// CHECK:   try_apply [[METHOD]]<[[OPEN_TYPE]]>([[RESULT_ADDR]], [[OPEN]])
//
// CHECK: bb1
// CHECK:  dealloc_stack [[RESULT]]
// CHECK:  destroy_addr %0
// CHECK:  return
//
// CHECK: bb2([[FAILURE:%.*]]: $Error):
// CHECK:  deinit_existential_addr [[RESULT]]
// CHECK:  dealloc_stack [[RESULT]]
// CHECK:  destroy_addr %0
// CHECK:  throw [[FAILURE]]
//
  try useP(p.upgrade())
}

// Same as above but for boxed existentials

extension Error {
  func returnOrThrowSelf() throws -> Self {
    throw self
  }
}

// CHECK-LABEL: sil hidden @_T019existential_erasure12errorHandlers5Error_psAC_pKF
func errorHandler(_ e: Error) throws -> Error {
// CHECK: bb0(%0 : $Error):
// CHECK:  debug_value %0 : $Error
// CHECK:  [[OPEN:%.*]] = open_existential_box %0 : $Error to $*[[OPEN_TYPE:@opened\(.*\) Error]]
// CHECK:  [[RESULT:%.*]] = alloc_existential_box $Error, $[[OPEN_TYPE]]
// CHECK:  [[ADDR:%.*]] = project_existential_box $[[OPEN_TYPE]] in [[RESULT]] : $Error
// CHECK:  [[FUNC:%.*]] = function_ref @_T0s5ErrorP19existential_erasureE17returnOrThrowSelf{{[_0-9a-zA-Z]*}}F
// CHECK:  try_apply [[FUNC]]<[[OPEN_TYPE]]>([[ADDR]], [[OPEN]])
//
// CHECK: bb1
// CHECK:  destroy_value %0 : $Error
// CHECK:  return [[RESULT]] : $Error
//
// CHECK: bb2([[FAILURE:%.*]] : $Error):
// CHECK:  dealloc_existential_box [[RESULT]]
// CHECK:  destroy_value %0 : $Error
// CHECK:  throw [[FAILURE]] : $Error
//
  return try e.returnOrThrowSelf()
}


// rdar://problem/22003864 -- SIL verifier crash when init_existential_addr
// references dynamic Self type
class EraseDynamicSelf {
  required init() {}

// CHECK-LABEL: sil hidden @_T019existential_erasure16EraseDynamicSelfC7factoryACXDyFZ : $@convention(method) (@thick EraseDynamicSelf.Type) -> @owned EraseDynamicSelf
// CHECK:  [[ANY:%.*]] = alloc_stack $Any
// CHECK:  init_existential_addr [[ANY]] : $*Any, $@dynamic_self EraseDynamicSelf
//
  class func factory() -> Self {
    let instance = self.init()
    let _: Any = instance
    return instance
  }
}
