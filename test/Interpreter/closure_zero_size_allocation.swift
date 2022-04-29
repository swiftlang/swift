// RUN: %target-run-simple-swift(-O)

// rdar://92418090

protocol P {
  var covariantSelfPropClosure: ((Self) -> Void) -> Void { get }
}
extension P {
  var covariantSelfPropClosure: ((Self) -> Void) -> Void { { $0(self) } }
}

struct S: P {}

let p: P = S()

p.covariantSelfPropClosure { _ in }
