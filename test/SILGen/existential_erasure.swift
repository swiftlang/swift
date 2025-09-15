
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name existential_erasure %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden [ossa] @$s19existential_erasure5PQtoPyyF : $@convention(thin) () -> () {
func PQtoP() {
  // CHECK: [[PQ_PAYLOAD:%.*]] = open_existential_addr immutable_access [[PQ:%.*]] : $*any P & Q to $*[[OPENED_TYPE:@opened\(.*, any P & Q\) Self]]
  // CHECK: [[P_PAYLOAD:%.*]] = init_existential_addr [[P:%.*]] : $*any P, $[[OPENED_TYPE]]
  // CHECK: copy_addr [[PQ_PAYLOAD]] to [init] [[P_PAYLOAD]]
  // CHECK: destroy_addr [[PQ]]
  // CHECK-NOT: destroy_addr [[P]]
  // CHECK-NOT: destroy_addr [[P_PAYLOAD]]
  // CHECK-NOT: deinit_existential_addr [[PQ]]
  // CHECK-NOT: destroy_addr [[PQ_PAYLOAD]]
  useP(makePQ())
}

// Make sure uninitialized existentials are properly deallocated when we
// have an early return.

// CHECK-LABEL: sil hidden [ossa] @$s19existential_erasure19openExistentialToP1yyAA1P_pKF
func openExistentialToP1(_ p: P) throws {
// CHECK: bb0(%0 : $*any P):
// CHECK:   [[OPEN:%.*]] = open_existential_addr immutable_access %0 : $*any P to $*[[OPEN_TYPE:@opened\(.*, any P\) Self]]
// CHECK:   [[RESULT:%.*]] = alloc_stack $any P
// CHECK:   [[FUNC:%.*]] = function_ref @$s19existential_erasure12throwingFuncSbyKF
// CHECK:   try_apply [[FUNC]]()
//
// CHECK: bb1([[SUCCESS:%.*]] : $Bool):
// CHECK:   [[METHOD:%.*]] = witness_method $[[OPEN_TYPE]], #P.downgrade : {{.*}}, [[OPEN]]
// CHECK:   [[RESULT_ADDR:%.*]] = init_existential_addr [[RESULT]] : $*any P, $[[OPEN_TYPE]]
// CHECK:   apply [[METHOD]]<[[OPEN_TYPE]]>([[RESULT_ADDR]], [[SUCCESS]], [[OPEN]])
// CHECK:   dealloc_stack [[RESULT]]
// CHECK:   return
//
// CHECK: bb2([[FAILURE:%.*]] : @owned $any Error):
// CHECK:   dealloc_stack [[RESULT]]
// CHECK:   throw [[FAILURE]]
//
  try useP(p.downgrade(throwingFunc()))
}

// CHECK-LABEL: sil hidden [ossa] @$s19existential_erasure19openExistentialToP2yyAA1P_pKF
func openExistentialToP2(_ p: P) throws {
// CHECK: bb0(%0 : $*any P):
// CHECK:   [[OPEN:%.*]] = open_existential_addr immutable_access %0 : $*any P to $*[[OPEN_TYPE:@opened\(.*, any P\) Self]]
// CHECK:   [[RESULT:%.*]] = alloc_stack $any P
// CHECK:   [[METHOD:%.*]] = witness_method $[[OPEN_TYPE]], #P.upgrade : {{.*}}, [[OPEN]]
// CHECK:   [[RESULT_ADDR:%.*]] = init_existential_addr [[RESULT]] : $*any P, $[[OPEN_TYPE]]
// CHECK:   try_apply [[METHOD]]<[[OPEN_TYPE]]>([[RESULT_ADDR]], [[OPEN]])
//
// CHECK: bb1
// CHECK:  dealloc_stack [[RESULT]]
// CHECK:  return
//
// CHECK: bb2([[FAILURE:%.*]]: @owned $any Error):
// CHECK:  deinit_existential_addr [[RESULT]]
// CHECK:  dealloc_stack [[RESULT]]
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

// CHECK-LABEL: sil hidden [ossa] @$s19existential_erasure12errorHandlerys5Error_psAC_pKF
func errorHandler(_ e: Error) throws -> Error {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $any Error):
// CHECK:  debug_value [[ARG]] : $any Error
// CHECK:  [[OPEN:%.*]] = open_existential_box [[ARG]] : $any Error to $*[[OPEN_TYPE:@opened\(.*, any Error\) Self]]
// CHECK:  [[FUNC:%.*]] = function_ref @$ss5ErrorP19existential_erasureE17returnOrThrowSelf{{[_0-9a-zA-Z]*}}F
// CHECK:  [[RESULT:%.*]] = alloc_existential_box $any Error, $[[OPEN_TYPE]]
// CHECK:  [[ADDR:%.*]] = project_existential_box $[[OPEN_TYPE]] in [[RESULT]] : $any Error
// CHECK:  store [[RESULT]] to [init] [[RESULT_BUF:%.*]] :
// CHECK:  try_apply [[FUNC]]<[[OPEN_TYPE]]>([[ADDR]], [[OPEN]])
//
// CHECK: bb1
// CHECK:  [[RESULT2:%.*]] = load [take] [[RESULT_BUF]]
// CHECK:  return [[RESULT2]] : $any Error
//
// CHECK: bb2([[FAILURE:%.*]] : @owned $any Error):
// CHECK:  [[RESULT3:%.*]] = load [take] [[RESULT_BUF]]
// CHECK:  dealloc_existential_box [[RESULT3]]
// CHECK:  throw [[FAILURE]] : $any Error
//
  return try e.returnOrThrowSelf()
}


// rdar://problem/22003864 -- SIL verifier crash when init_existential_addr
// references dynamic Self type
class EraseDynamicSelf {
  required init() {}

// CHECK-LABEL: sil hidden [ossa] @$s19existential_erasure16EraseDynamicSelfC7factoryACXDyFZ : $@convention(method) (@thick EraseDynamicSelf.Type) -> @owned EraseDynamicSelf
// CHECK:  [[ANY:%.*]] = alloc_stack $Any
// CHECK:  init_existential_addr [[ANY]] : $*Any, $@dynamic_self EraseDynamicSelf
//
  class func factory() -> Self {
    let instance = self.init()
    let _: Any = instance
    return instance
  }
}
