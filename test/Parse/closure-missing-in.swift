// RUN: %target-swift-frontend -parse -verify -swift-version 5 %s

func test<T>(make: () -> [T], consume: (T) -> Void) { consume(make()[0]) }

func main1() {
  test(make: { () -> [Int] // expected-cparser-error@+1 {{expected 'in' after the closure signature}} expected-astgen-error {{expected 'in' in closure signature}}
    return [3]
  }, consume: { _ in })
}


// Resync path: there are tokens before `in` — should diagnose once, then recover.
func main2() {
  _ = { () -> Int
    0 // expected-cparser-error {{unexpected tokens prior to 'in'}} expected-astgen-error {{unexpected code '0' in closure signature}}
    in
    1
  }
}

func main3() {
  _ = { x, y -> Int // expected-astgen-error {{expected 'in' in closure signature}}
    x + y // expected-cparser-error {{expected 'in' after the closure signature}}
  }
}

func ok() {
  _ = { (x: Int) -> Int in x + 1 }
}
