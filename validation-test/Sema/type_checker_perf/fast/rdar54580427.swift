// RUN: %scale-test --begin 1 --end 20 --step 1 --select NumLeafScopes %s -Xfrontend=-solver-expression-time-threshold=1
// REQUIRES: asserts,no_asan

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
