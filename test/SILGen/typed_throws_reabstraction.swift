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
