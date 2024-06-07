// RUN: %target-run-simple-swift

// REQUIRES: executable_test

func callee<X: P1, T: P2, U: P3, V: P4>(x: X, t: T, u: U, v: V)
    -> (any P1, any P2, any P3, any P4) {
  return (x, t, u, v)
}

protocol P1 { var f1: Bool { get } }
protocol P2 { var f2: Bool { get } }
protocol P3 { var f3: Bool { get } }
protocol P4 { var f4: Bool { get } }

func packFunction<X, each T, each U, each V>(x: X,
                                             ts: repeat each T,
                                             us: repeat each U,
                                             vs: repeat each V)
    where X: P1,
          (repeat (each T, each U)): Any,
          repeat each T: P2,
          repeat each U: P3,
          repeat each V: P4 {

  var result: [(any P1, any P2, any P3, any P4)] = []
  var bools: [Bool] = []

  for t in repeat each ts {
    for u in repeat each us {
      for v in repeat each vs {

        // Local function
        do {
          func localFunc() {
            result.append(callee(x: x, t: t, u: u, v: v))
            bools.append(true && x.f1 && t.f2 && u.f3 && v.f4)
          }

          localFunc()
          let fn = localFunc
          fn()

          let closure = { localFunc() }
          closure()
        }

        // Generic local function
        do {
          func localFunc<Y: P1>(_ y: Y) {
            result.append(callee(x: y, t: t, u: u, v: v))
            bools.append(true && y.f1 && t.f2 && u.f3 && v.f4)
          }

          localFunc(x)
          let fn: (X) -> () = localFunc
          fn(x)

          let closure = { localFunc(x) }
          closure()
        }
      }
    }
  }

  assert(result.count == bools.count)

  for (r, b) in zip(result, bools) {
    assert(b == (r.0.f1 && r.1.f2 && r.2.f3 && r.3.f4))
  }
}

struct S1: P1 { var f1: Bool }
struct S2: P2 { var f2: Bool }
struct S3: P3 { var f3: Bool }
struct S4: P4 { var f4: Bool }

packFunction(x: S1(f1: true),
             ts: S2(f2: true), S2(f2: true),
             us: S3(f3: true), S3(f3: false),
             vs: S4(f4: false), S4(f4: false), S4(f4: true), S4(f4: true))
