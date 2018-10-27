// RUN: %target-swift-frontend %s -emit-sil -verify

func singleBlock() -> Int {
  _ = 0
} // expected-error {{missing return in a function expected to return 'Int'}}

func singleBlock2() -> Int {
  var y = 0 
  y += 1
} // expected-error {{missing return in a function expected to return 'Int'}}

enum NoCasesButNotNever {}

func diagnoseNoCaseEnumMissingReturn() -> NoCasesButNotNever {
} // expected-error {{function with uninhabited return type 'NoCasesButNotNever' is missing call to another never-returning function on all paths}} 

func diagnoseNeverMissingBody() -> Never {
} // expected-error {{function with uninhabited return type 'Never' is missing call to another never-returning function on all paths}} 

_ = { () -> Never in
}() // expected-error {{closure with uninhabited return type 'Never' is missing call to another never-returning function on all paths}}-

func diagnoseNeverWithBody(i : Int) -> Never {
  if (i == -1) {
    print("Oh no!")
  } else {
    switch i {
    case 0:
      exit()
    case 1:
      fatalError()
    default:
      repeat { } while true 
    } 
  }
} // expected-error {{function with uninhabited return type 'Never' is missing call to another never-returning function on all paths}}

class MyClassWithClosure {
  var f : (_ s: String) -> String = { (_ s: String) -> String in } // expected-error {{missing return in a closure expected to return 'String'}}
}

func multipleBlocksSingleMissing(b: Bool) -> (String, Int) {
  var y = 0 
  if b {
    return ("a", 1)
  } else if (y == 0) {
    y += 1
  }
} // expected-error {{missing return in a function expected to return '(String, Int)'}}

func multipleBlocksAllMissing(x: Int) -> Int {
  var y : Int = x + 1 
  while (y > 0 ) {
    y -= 1
    break
  }
  var x = 0
  x += 1
} // expected-error {{missing return in a function expected to return 'Int'}}

@_silgen_name("exit") func exit () -> Never

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

class TuringMachine {
  func halt() -> Never {
    repeat { } while true
  }
}

func diagnose_missing_return_no_error_after_noreturn_method() -> Int {
  TuringMachine().halt()
} // no error

func whileLoop(flag: Bool) -> Int {
  var b = 1
  while (flag) {
    if b == 3 {
      return 3
    }
    b += 1
  }
} //expected-error {{missing return in a function expected to return 'Int'}}

struct S {}
extension S:ExpressibleByStringLiteral {
  init!(stringLiteral:String) {
  } // no error
}

func whileTrueLoop() -> Int {
  var b = 1
  while (true) {
    if b == 3 {
      return 3
    }
    b += 1
  } // no-error
}

func testUnreachableAfterNoReturn(x: Int) -> Int {
  exit(); // expected-note{{call to never-returning function terminates the program}}
  return x; // expected-warning {{will never be executed}}
}

func testUnreachableAfterNoReturnInADifferentBlock() -> Int {
  let x:Int = 5
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
  let x:Int = 5
  exit(); // expected-note{{call to never-returning function terminates the program}}
  exit(); // expected-warning {{will never be executed}}
  return x
}

func testUnreachableAfterNoReturnMethod() -> Int {
  TuringMachine().halt(); // expected-note{{call to never-returning function terminates the program}}
  return 0; // expected-warning {{will never be executed}}
}

func testCleanupCodeEmptyTuple(fn: @autoclosure () -> Bool = false,
          message: String = "",
          file: String = #file,
          line: Int = #line) {
  if true {
    exit()
  }
} // no warning

protocol InitProtocol {
  init(_ x: Int)
}

struct StructWithIUOinit : InitProtocol {
  init!(_ x: Int) {  } // no missing-return error
}

