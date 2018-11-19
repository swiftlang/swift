// RUN: %target-swift-emit-sil -enable-sil-ownership %s -o /dev/null -verify

//===--- Function declaration with uninhabited parameter type
                                   
func foo(baz: Never) -> Int { // expected-note {{the function body will never be executed. remove the function body to dismiss the warning}}
  print("I can't be called!") // expected-warning{{will never be executed}}
  return 0
}

func bar(baz: Never) -> Int {} // ok
