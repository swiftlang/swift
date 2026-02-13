// RUN: %target-swift-emit-silgen %s | %FileCheck %s
func test2() throws { // Not OK
  // The literal closure below is in a generic error context, even though
  // it statically throws `any Error`, leading it to be emitted with an
  // indirect `any Error` out parameter.
  try call { () throws in
  // CHECK-LABEL: sil{{.*}} @{{.*}}5test2{{.*}}fU_
    do {
      try gorble()
    }
    // Make sure we stop borrowing the error value before forwarding it into
    // the error return slot.
    // CHECK: bb{{.*}}([[ERROR:%.*]] : @owned $any Error):
    // CHECK:      [[ERROR_BORROW:%.*]] = begin_borrow [[ERROR]]
    // CHECK: bb{{[0-9]+}}:
    // CHECK: bb{{[0-9]+}}:
    // CHECK:      end_borrow [[ERROR_BORROW]]
    // CHECK-NEXT: store [[ERROR]] to [init]
    // CHECK-NEXT: throw_addr

    catch is E1 { /* ignore */ }
  }
}

func call<E: Error, R>(
  _ body: () throws(E) -> R
) throws(E) -> R {
  try body()
}

struct E1: Error {}

func gorble() throws {}

func test1() throws { // OK
  try call { () throws in
    try gorble()
  }
}

// rdar://169150387
func test3<E: Error>(e: E) {
  // CHECK-LABEL: sil private [ossa] @$s{{.+}}5test31eyx_ts5ErrorRzlFyyxYKcfU_ : $@convention(thin) <E where E : Error> (@in_guaranteed E) -> @error any Error {
  // CHECK:      bb0([[V1:%.+]] : @closureCapture $*E):
  // CHECK:        [[V2:%.+]] = alloc_stack $E
  // CHECK-NEXT:   copy_addr [[V1]] to [init] [[V2]]
  // CHECK:        [[V3:%.+]] = alloc_stack $any Error
  // CHECK-NEXT:   [[V4:%.+]] = alloc_stack $E
  // CHECK-NEXT:   copy_addr [take] [[V2]] to [init] [[V4]]
  // CHECK-NEXT:   [[V5:%.+]] = alloc_existential_box $any Error, $E
  // CHECK-NEXT:   [[V6:%.+]] = project_existential_box $E in [[V5]]
  // CHECK-NEXT:   store [[V5]] to [init] [[V3]]
  // CHECK-NEXT:   copy_addr [take] [[V4]] to [init] [[V6]]
  // CHECK-NEXT:   [[V7:%.+]] = load [take] [[V3]]
  // CHECK-NEXT:   dealloc_stack [[V4]]
  // CHECK-NEXT:   dealloc_stack [[V3]]
  // CHECK-NEXT:   dealloc_stack [[V2]]
  // CHECK-NEXT:   throw [[V7]]
  // CHECK-NEXT: } // end sil function '$s{{.+}}5test31eyx_ts5ErrorRzlFyyxYKcfU_'
  let _: () throws -> Void = { () throws(E) -> Void in
    throw e
  }
}
