// RUN: %target-run-simple-swift(-Xfrontend -disable-callee-allocated-coro-abi -enable-experimental-feature CoroutineAccessors) | %FileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: executable_test

struct AirOr : Error {
}

struct Thrower {
  mutating func doit() throws {
    throw AirOr()
  }
}

struct S {
  var _thrower = Thrower()
  var thrower: Thrower {
    read {
      yield _thrower
    }
    modify {
      // CHECK: first
      print("first")
      yield &_thrower
      // CHECK: also ran
      print("also ran")
    }
  }
}

func doit() {
  do {
    var s = S()
    try s.thrower.doit()
  } catch let error {
    // CHECK: AirOr
    print(error)
  }
}

doit()
