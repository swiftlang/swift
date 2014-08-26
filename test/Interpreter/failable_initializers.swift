// RUN: %target-run-simple-swift | FileCheck %s

class Canary {
  deinit { println("died") }
}

struct GuineaPig {
  let canary: Canary

  init?(live: Bool) {
    canary = Canary()
    if !live { return nil }
  }
}

// CHECK: it's alive
if let x = GuineaPig(live: true) {
  println("it's alive")
} else {
  println("it's dead")
}
// CHECK-NEXT: died

// CHECK-NEXT: died
// CHECK-NEXT: it's dead
if let y = GuineaPig(live: false) {
  println("it's alive")
} else {
  println("it's dead")
}

// CHECK-NEXT: done
println("done")
