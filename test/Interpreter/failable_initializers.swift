// RUN: %target-run-simple-swift | FileCheck %s

class Canary {
  deinit { println("died") }
}

struct GuineaPig {
  let canary: Canary

  init() { canary = Canary() }

  init?(live: Bool) {
    canary = Canary()
    if !live { return nil }
  }

  init?(failBefore: Bool) {
    if failBefore { return nil }
    canary = Canary()
  }

  init?(delegateFailure: Bool, failBefore: Bool, failAfter: Bool) {
    if failBefore { return nil }
    self.init(live: !delegateFailure)
    if failAfter { return nil }
  }

  init?(alwaysFail: Void) {
    return nil
  }
}

// CHECK: it's alive
if let x = GuineaPig(live: true) {
  println("it's alive")
} else {
  println("it's dead")
}
// CHECK-NEXT: died

println("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let y = GuineaPig(live: false) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
if let a = GuineaPig(failBefore: false) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: it's dead
if let b = GuineaPig(failBefore: true) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: it's dead
if let c = GuineaPig(alwaysFail: ()) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: it's dead
if let d = GuineaPig(delegateFailure: false, failBefore: true, failAfter: false) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let e = GuineaPig(delegateFailure: true, failBefore: false, failAfter: false) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let f = GuineaPig(delegateFailure: false, failBefore: false, failAfter: true) {
  println("it's alive")
} else {
  println("it's dead")
}

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
if let g = GuineaPig(delegateFailure: false, failBefore: false, failAfter: false) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

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
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
if let r = Chimera(failBefore: false) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let s = Chimera(failBetween: true) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
if let t = Chimera(failBetween: false) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: died
// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let u = Chimera(failAfter: true) {
  println("it's alive")
} else {
  println("it's dead")
}

println("--") // CHECK-NEXT: --

// CHECK-NEXT: it's alive
// CHECK-NEXT: died
// CHECK-NEXT: died
if let v = Chimera(failAfter: false) {
  println("it's alive")
} else {
  println("it's dead")
}

// CHECK-NEXT: done
println("done")

