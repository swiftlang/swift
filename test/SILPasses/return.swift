// RUN: %target-swift-frontend %s -emit-sil -verify

func singleBlock() -> Int {
  var _ = 0
} // expected-error {{missing return in a function expected to return 'Int'}}

func singleBlock2() -> Int {
  var y = 0 
  y++
} // expected-error {{missing return in a function expected to return 'Int'}}

class MyClassWithClosure {
  var f : (s: String) -> String = { (s: String) -> String in } // expected-error {{missing return in a closure expected to return 'String'}}
}

func multipleBlocksSingleMissing(b: Bool) -> (String, Int) {
  var y = 0 
  if b {
    return ("a", 1)
  } else if (y == 0) {
    y++
  }
} // expected-error {{missing return in a function expected to return '(String, Int)'}}

func multipleBlocksAllMissing(x: Int) -> Int {
  var y : Int = x + 1 
  while (y > 0 ) {
    --y;
    break;
  }
  var x = 0
  x++
} // expected-error {{missing return in a function expected to return 'Int'}}

@noreturn func MYsubscriptNonASCII(idx: Int) -> UnicodeScalar {
} // no-warning

@noreturn @asmname("exit") func exit ()->()
@noreturn func tryingToReturn (x: Bool) -> () {
  if x {
    return // expected-error {{return from a 'noreturn' function}}
  }
  exit()
}

@noreturn func implicitReturnWithinNoreturn() {
  var _ = 0
}// expected-error {{return from a 'noreturn' function}}

func diagnose_missing_return_in_the_else_branch(i: Bool) -> Int {
  if (i) {
    exit() 
  } 
} // expected-error {{missing return in a function expected to return 'Int'}}

func diagnose_missing_return_no_error_after_noreturn(i: Bool) -> Int {
  if (i) {
    exit()
  } else {
    exit()
  }
} // no error

func whileLoop(flag: Bool) -> Int {
  var b = 1;
  while (flag) {
    if b == 3 {
      return 3
    }
    b++  
  }
} //expected-error {{missing return in a function expected to return 'Int'}}

func whileTrueLoop() -> Int {
  var b = 1;
  while (true) {
    if b == 3 {
      return 3
    }
    b++  
  } // no-error
}

func testUnreachableAfterNoReturn(x: Int) -> Int {
  exit(); // expected-note{{a call to a noreturn function}}
  return x; // expected-warning {{will never be executed}}
}

func testUnreachableAfterNoReturnInADifferentBlock() -> Int {
  let x:Int = 5;
  if true {  // expected-note {{condition always evaluates to true}}
    exit(); 
  }
  return x; // expected-warning {{will never be executed}}
}

func testReachableAfterNoReturnInADifferentBlock(x: Int) -> Int {
  if x == 5 {
    exit();
  }
  return x; // no warning
}

func testUnreachableAfterNoReturnFollowedByACall() -> Int {
  let x:Int = 5;
  exit(); // expected-note{{a call to a noreturn function}}
  exit(); // expected-warning {{will never be executed}}
  return x; 
}

func testCleanupCodeEmptyTuple(@autoclosure fn: () -> Bool = false,
          message: String = "",
          file: String = __FILE__,
          line: Int = __LINE__) {
  if true {
    exit()
  }
} // no warning
