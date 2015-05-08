// RUN: %target-run-simple-swift | FileCheck %s

class Canary {
  deinit { print("died") }
}

struct GuineaPig {
  let canary: Canary

  init() { canary = Canary() }

  init?(fail: Bool) {
    canary = Canary()
    if fail { return nil }
  }

  init?(failBefore: Bool) {
    if failBefore { return nil }
    canary = Canary()
  }

  init?(delegateFailure: Bool, failBefore: Bool, failAfter: Bool) {
    if failBefore { return nil }
    self.init(fail: delegateFailure)
    if failAfter { return nil }
  }

  init?(alwaysFail: Void) {
    return nil
  }
}

// CHECK: it's alive
if let x = GuineaPig(fail: false) {
  print("it's alive")
} else {
  print("it's dead")
}
// CHECK-NEXT: died

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let y = GuineaPig(fail: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
if let a = GuineaPig(failBefore: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's dead
if let b = GuineaPig(failBefore: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's dead
if let c = GuineaPig(alwaysFail: ()) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's dead
if let d = GuineaPig(delegateFailure: false, failBefore: true, failAfter: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let e = GuineaPig(delegateFailure: true, failBefore: false, failAfter: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let f = GuineaPig(delegateFailure: false, failBefore: false, failAfter: true) {
  print("it's alive")
} else {
  print("it's dead")
}

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
if let g = GuineaPig(delegateFailure: false, failBefore: false, failAfter: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

struct Chimera {
  let canary: Canary
  let guineaPig: GuineaPig

  init?(failBefore: Bool) {
    if failBefore { return nil }
    canary = Canary()
    guineaPig = GuineaPig()
  }

  init?(failBetween: Bool) {
    canary = Canary()
    if failBetween { return nil }
    guineaPig = GuineaPig()
  }

  init?(failAfter: Bool) {
    canary = Canary()
    guineaPig = GuineaPig()
    if failAfter { return nil }
  }
}

// CHECK-NEXT: it's dead
if let q = Chimera(failBefore: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
if let r = Chimera(failBefore: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let s = Chimera(failBetween: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
if let t = Chimera(failBetween: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let u = Chimera(failAfter: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
if let v = Chimera(failAfter: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

class Bear {
  let x: Canary

  init?(fail: Bool) {
    x = Canary()
    if fail { return nil }
  }

  convenience init?(delegateFailure: Bool, failAfter: Bool) {
    self.init(fail: delegateFailure)
    if failAfter { return nil }
  }
}

final class PolarBear: Bear {
  let y: Canary

  override init?(fail: Bool) {
    y = Canary()
    super.init(fail: fail)
  }

  init?(chainFailure: Bool, failAfter: Bool) {
    y = Canary()
    super.init(fail: chainFailure)
    if failAfter { return nil }
  }
}

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
if let ba = Bear(fail: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let bb = Bear(fail: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
if let bc = Bear(delegateFailure: false, failAfter: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let bd = Bear(delegateFailure: false, failAfter: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let bd = Bear(delegateFailure: true, failAfter: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
if let be = PolarBear(chainFailure: false, failAfter: false) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let bf = PolarBear(chainFailure: false, failAfter: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let bf = PolarBear(chainFailure: true, failAfter: true) {
  print("it's alive")
} else {
  print("it's dead")
}

print("--") // CHECK-NEXT: --

protocol FailableOnDemand {
  init?(fail: Bool)
}

extension GuineaPig: FailableOnDemand {}
extension PolarBear: FailableOnDemand {}

struct IUOGuineaPig : FailableOnDemand {
  let canary: Canary

  init() { canary = Canary() }

  init!(fail: Bool) {
    canary = Canary()
    if fail { return nil }
  }
}

final class IUOPolarBear: Bear, FailableOnDemand {
  let y: Canary

  override init!(fail: Bool) {
    y = Canary()
    super.init(fail: fail)
  }
}

func tryInitFail<T: FailableOnDemand>(_: T.Type, #fail: Bool) {
  if let x = T(fail: fail) {
    print("it's alive")
  } else {
    print("it's dead")
  }
}

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
tryInitFail(GuineaPig.self, fail: false)

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
tryInitFail(GuineaPig.self, fail: true)

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
tryInitFail(PolarBear.self, fail: false)

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: died
// CHECK-NEXT: it's dead
tryInitFail(PolarBear.self, fail: true)

// CHECK-NEXT: it's alive
tryInitFail(IUOGuineaPig.self, fail: false)
// CHECK-NEXT: died

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
tryInitFail(IUOGuineaPig.self, fail: true)

print("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
tryInitFail(IUOPolarBear.self, fail: false)

print("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: died
// CHECK-NEXT: it's dead
tryInitFail(IUOPolarBear.self, fail: true)

// CHECK-NEXT: done
print("done")


