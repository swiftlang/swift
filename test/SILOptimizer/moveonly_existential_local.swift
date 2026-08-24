// RUN: %target-swift-frontend %s -module-name main -parse-as-library -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend %s -module-name main -parse-as-library -O -emit-sil -o /dev/null


protocol P: ~Copyable {
  func f()
}

// A payload with a deinit: its subelement count exceeds the existential's.
struct WithDeinit<T>: ~Copyable, P {
  var p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  func f() {}
  deinit { p.deallocate() }
}

// Several stored properties, so the payload count exceeds 1 for another reason.
struct MultiField: ~Copyable, P {
  var a: Int
  var b: Int
  init() { a = 0; b = 0 }
  func f() {}
  deinit {}
}

func bindGenericPayload() {
  let e: any P & ~Copyable = WithDeinit<Int>()
  e.f()
}

func bindMultiFieldPayload() {
  let e: any P & ~Copyable = MultiField()
  e.f()
}

func bindAndBorrowRepeatedly() {
  let e: any P & ~Copyable = WithDeinit<Int>()
  e.f()
  e.f()
}
