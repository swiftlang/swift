// RUN: %target-parse-verify-swift

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

func test_UnsafePointer_null() {
  let ptr1 = UnsafePointer<Int>.null()
  // expected-error@-1 {{'null()' is unavailable: use 'nil' literal instead}}
  let ptr2 = UnsafeMutablePointer<Int>.null()
  // expected-error@-1 {{'null()' is unavailable: use 'nil' literal instead}}
}

func test_COpaquePointer_null() {
  let ptr1 = COpaquePointer.null()
  // expected-error@-1 {{'null()' is unavailable: use 'nil' literal instead}}
}

func test_CFunctionPointer_null() {
  let ptr1 = CFunctionPointer<() -> ()>.null()
  // expected-error@-1 {{'null()' is unavailable: use 'nil' literal instead}}
}

func test_AutoreleasingUnsafeMutablePointer_null() {
  let ptr1 = AutoreleasingUnsafeMutablePointer<AnyObject>.null()
  // expected-error@-1 {{'null()' is unavailable: use 'nil' literal instead}}
}

