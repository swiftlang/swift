// RUN: %target-typecheck-verify-swift

func bar(_ f: @escaping (Int) -> Int) -> ((Int) -> Int) -> Int {
  let r : ((Int) -> Int) -> Int =
  {(g: (_ r1 : Int) -> Int) in
      g(f(2))
  }
  return r
}

// bad 1
let q1 = bar()
{7 * $0}
{5 * $0} // expected-error {{double trailing closures}}

// bad 2 (note that () can't be on its own line
let q2 =
bar {7 * $0} ()
{5 * $0} // expected-error {{double trailing closures}}
