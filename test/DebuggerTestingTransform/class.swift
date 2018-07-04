// RUN: %target-swift-frontend -debugger-testing-transform -Xllvm -sil-full-demangle -emit-sil -module-name M %s | %FileCheck %s -check-prefix=CHECK-SIL

class C1 {
  var x: Int = 0
  init() {
    // CHECK-SIL: // closure #1 () -> () in M.C1.init() -> M.C1
    // CHECK-SIL: string_literal utf8 "x"
    // CHECK-SIL: // function_ref Swift._stringForPrintObject(Any) -> Swift.String
    // CHECK-SIL: // function_ref Swift._debuggerTestingCheckExpect(Swift.String, Swift.String) -> ()
    // CHECK-SIL: // end sil function
    self.x = 1
  }
}
