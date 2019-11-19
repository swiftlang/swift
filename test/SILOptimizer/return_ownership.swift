// RUN: %target-swift-frontend %s -emit-sil -verify

class TuringMachine {
  func halt() -> Never {
    repeat { } while true
  }
}

func diagnose_missing_return_no_error_after_noreturn_method() -> Int {
  TuringMachine().halt()
} // no error

func testUnreachableAfterNoReturnMethod() -> Int {
  TuringMachine().halt(); // expected-note{{a call to a never-returning function}}
  return 0; // expected-warning {{will never be executed}}
}

