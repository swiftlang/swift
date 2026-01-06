// RUN: %target-swift-frontend -parse -verify -swift-version 5 %s

func test<T>(make: () -> [T], consume: (T) -> Void) { consume(make()[0]) }

func main1() {
  test(make: { () -> [Int] // expected-error@+1 {{expected 'in' after the closure signature}}
    return [3]
  }, consume: { _ in })
}


// Resync path: there are tokens before `in` — should diagnose once, then recover.
func main2() {
  _ = { () -> Int
    0 // expected-error {{unexpected tokens prior to 'in'}}
    in
    1
  }
}

func main3() {
  _ = { x, y -> Int
    x + y // expected-error {{expected 'in' after the closure signature}}
  }
}

func ok() {
  _ = { (x: Int) -> Int in x + 1 }
}
