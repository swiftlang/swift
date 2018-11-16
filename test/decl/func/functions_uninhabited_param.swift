// RUN: %target-swift-emit-silgen

//===--- Function declaration with uninhabited parameter type
                                   
func foo(baz: Never) -> Int {
  print("I can't be called!")
  return 0 // expected-warning{{will never be executed}}
}

func bar(baz: Never) -> Int {} // ok
