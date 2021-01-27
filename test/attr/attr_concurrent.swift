// RUN: %target-swift-frontend -typecheck -verify %s -enable-experimental-concurrency
// REQUIRES: concurrency

// Concurrent attribute on a function type.
func f(_ fn: @concurrent (Int) -> Int) { }

// Okay to overload @concurrent vs. not concurrent
func f(_ fn: (Int) -> Int) { }


func acceptsConcurrent(_ fn: @concurrent (Int) -> Int) { }
func acceptsNonConcurrent(_ fn: (Int) -> Int) { }

func passingConcurrentOrNot(
  _ cfn: @concurrent (Int) -> Int,
  ncfn: (Int) -> Int // expected-note{{parameter 'ncfn' is implicitly non-concurrent}}{{9-9=@concurrent }}
) {
  // Okay due to overloading
  f(cfn)
  f(ncfn)

  acceptsConcurrent(cfn) // okay
  acceptsConcurrent(ncfn) // expected-error{{passing non-concurrent parameter 'ncfn' to function expecting a @concurrent closure}}
  acceptsNonConcurrent(cfn) // okay
  acceptsNonConcurrent(ncfn) // okay
}

func closures() {
  // Okay, inferring @concurrent
  acceptsConcurrent { $0 }
  acceptsConcurrent({ $0 })
  acceptsConcurrent({ i in i })
  acceptsConcurrent({ (i: Int) -> Int in
      print(i)
      return i
    })

  let closure1 = { $0 + 1 } // inferred to be non-concurrent
  acceptsConcurrent(closure1) // expected-error{{converting non-concurrent function value to '@concurrent (Int) -> Int' may introduce data races}}
}
