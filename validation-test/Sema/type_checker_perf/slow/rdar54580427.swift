// FIXME: This should be linear instead of exponential.
// RUN: %scale-test --begin 1 --end 10 --step 1 --select NumLeafScopes --invert-result %s
// REQUIRES: asserts

enum Val {
  case d([String: Val])
  case f(Double)
}

struct X {
  var x : Float
}

extension X {
  func val() -> Val {
    return Val.d([
%for i in range(0, N):
      "x": .f(Double(x)),
%end
    ])
  }
}
