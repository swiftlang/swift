// RUN: %target-swift-emit-sil -enable-sil-ownership %s -o /dev/null -verify

//===--- Function declaration with uninhabited parameter type
                                   
func foo(baz: Never) -> Int { // expected-note {{'baz' is uninhabited, so this function body can never be executed}}
  print("I can't be called!") // expected-warning{{will never be executed}}
  return 0
}

func bar(baz: Never) -> Int {} // ok
