// RUN: %target-typecheck-verify-swift %s

// RUN: %target-typecheck-verify-swift -Wwarning PerformanceHints -verify-additional-prefix perfhints-
// RUN: %target-typecheck-verify-swift -Wwarning UntypedThrows -verify-additional-prefix perfhints-

enum MyError: Error {
case failed
}

// expected-perfhints-warning@+2{{untyped throws performs heap allocation on each 'throw'}}
// expected-perfhints-note@+1{{consider adding a thrown error type with '(type)'}}{{28-28=(<#any Error#>)}}
func untypedThrows() throws { }

// expected-perfhints-warning@+2{{untyped throws performs heap allocation on each 'throw'}}
// expected-perfhints-note@+1{{consider adding a thrown error type with '(type)'}}{{29-29=(<#any Error#>)}}
typealias FnType = () throws -> Void

func untypedThrowsInBody() {
  // expected-perfhints-warning@+2{{untyped throws performs heap allocation on each 'throw'}}
// expected-perfhints-note@+1{{consider adding a thrown error type with '(type)'}}{{12-12=(<#any Error#>)}}
  do throws {
    throw MyError.failed
  } catch {
  }

  // expected-perfhints-warning@+2{{untyped throws performs heap allocation on each 'throw'}}
  // expected-perfhints-note@+1{{consider adding a thrown error type with '(type)'}}{{19-19=(<#any Error#>)}}
  _ = { (x) throws in x + 1 }
}

struct SomeStruct {
  // expected-perfhints-warning@+2{{untyped throws performs heap allocation on each 'throw'}}
// expected-perfhints-note@+1{{consider adding a thrown error type with '(type)'}}{{16-16=(<#any Error#>)}}
  init() throws { }

  var value: Int {
    // expected-perfhints-warning@+2{{untyped throws performs heap allocation on each 'throw'}}
  // expected-perfhints-note@+1{{consider adding a thrown error type with '(type)'}}{{15-15=(<#any Error#>)}}
    get throws {
      0
    }
  }
}
