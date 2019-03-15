// RUN: %target-typecheck-verify-swift

struct SimpleCallable {
  call(_ x: Int) -> Int {
    return x
  }
}

struct NoRedefinitionErrors {
  call(_ x: Int) -> Int {
    return x
  }
  call(x: Int) -> Int {
    return x
  }
  call(_ x: Int, y: Int) -> Int {
    return x
  }
}

struct RedefinitionErrors {
  // expected-note @+1 2 {{'call' previously declared here}}
  call(_ x: Int) -> Int {
    return x
  }
  // expected-error @+1 {{invalid redeclaration of 'call'}}
  call(_ y: Int) -> Int {
    return y
  }
  // expected-error @+1 {{invalid redeclaration of 'call'}}
  func call(_ x: Int) -> Int {
    return x
  }
}

struct ParseErrors {
  // expected-error @+1 {{expected '->' after function parameter tuple}} {{22-23= ->}}
  func call(_ x: Int): Int {
    return x
  }

  // expected-error @+1 {{expected type for function result}}
  func call(_ x: Int) -> {}
}

class StaticSpellingErrors {
  // expected-error @+1 {{'call' member cannot be marked 'static'}}
  static call(_ x: Int) {}
  // expected-error @+1 {{expected '{' in class}}
  class call(_ x: Int) {}
}

class StaticSpellingOkay_Static {
  static func call(_ x: Int) {}
}
class StaticSpellingOkay_Class {
  class func call(_ x: Int) {}
}

protocol ParseErrorsProtocol {
  // expected-error @+1 {{default argument not permitted in a protocol method}}
  func call(_ x: Int = 1) -> Int
}

// Test context sensitive parsing.
// "call" can appear in declaration or expression contexts.
struct ContextSensitiveParsing {
  // declaration
  call(_ fn: () -> Void) {
    // expression
    call() {}
    // expression
    call {}

    struct U {
      // declaration
      call(x: Int) {}

      // error
      // expected-error @+1 {{expected '(' for 'call' member parameters}} {{11-11=()}}
      call {}
    }
  }
}

// Global function. Used below.
func call(_ fn: () -> Void) {}

// expression
call() {}
