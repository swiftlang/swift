// RUN: %target-parse-verify-swift

func test_UnsafePointer_null() {
  let ptr1 = UnsafePointer<Int>.null()
  // expected-error@-1 {{'null()' is unavailable: use 'nil' literal instead}}
  let ptr2 = UnsafeMutablePointer<Int>.null()
  // expected-error@-1 {{'null()' is unavailable: use 'nil' literal instead}}
}

