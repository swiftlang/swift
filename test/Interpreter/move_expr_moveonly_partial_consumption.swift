// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

@main struct App { static func main() {
  test1()
}}

func barrier() { print("barrier") }

struct Ur : ~Copyable {
  var name: String
  init(named name: String) {
    self.name = name
    print("hi", name)
  }
  deinit {
    print("bye", name)
  }
}
func take(_ u: consuming Ur) {}

struct Pair : ~Copyable {
  var u1: Ur
  var u2: Ur
  init(named name: String) {
    u1 = .init(named: "\(name).u1")
    u2 = .init(named: "\(name).u2")
  }
}
func take(_ u: consuming Pair) {}

struct Quad : ~Copyable {
  var p1: Pair
  var p2: Pair
  init(named name: String) {
    p1 = .init(named: "\(name).p1")
    p2 = .init(named: "\(name).p2")
  }
}
func take(_ u: consuming Quad) {}

func test1() {
do {
  let q1 = Quad(named: "\(#function).q1")
  barrier()
  // CHECK: barrier
  // CHECK: bye test1().q1.p1.u1
  // CHECK: bye test1().q1.p1.u2
  // CHECK: bye test1().q1.p2.u1
  // CHECK: bye test1().q1.p2.u2
}

  let q2 = Quad(named: "\(#function).q2")
  take(q2.p2.u2)
  take(q2.p2.u1)
  barrier()
  take(q2.p1.u2)
  take(q2.p1.u1)
  // CHECK: bye test1().q2.p2.u2
  // CHECK: bye test1().q2.p2.u1
  // CHECK: barrier
  // CHECK: bye test1().q2.p1.u2
  // CHECK: bye test1().q2.p1.u1

  let q3 = Quad(named: "\(#function).q3")
  _ = consume q3.p2.u2
  _ = consume q3.p2.u1
  _ = consume q3.p1.u2
  barrier()
  _ = consume q3.p1.u1
  // CHECK: bye test1().q3.p2.u2
  // CHECK: bye test1().q3.p2.u1
  // CHECK: bye test1().q3.p1.u2
  // CHECK: barrier
  // CHECK: bye test1().q3.p1.u1

  let q4 = Quad(named: "\(#function).q4")
  _ = consume q4.p1.u1
  barrier()
  _ = consume q4.p2.u1
  _ = consume q4.p1.u2
  _ = consume q4.p2.u2
  // CHECK: bye test1().q4.p1.u1
  // CHECK: barrier
  // CHECK: bye test1().q4.p2.u1
  // CHECK: bye test1().q4.p1.u2
  // CHECK: bye test1().q4.p2.u2

do {
  let q5 = Quad(named: "\(#function).q5")
  take(q5.p1.u1)
  _ = consume q5.p2.u1
  _ = consume q5.p1.u2
  take(q5.p2.u2)
  // CHECK: bye test1().q5.p1.u1
  // CHECK: bye test1().q5.p2.u1
  // CHECK: bye test1().q5.p1.u2
  // CHECK: bye test1().q5.p2.u2
}
do {
  let q6 = Quad(named: "\(#function).q6")
  if Bool.random() {
    take(q6.p1.u1)
    _ = consume q6.p2.u1
  } else {
    _ = consume q6.p1.u2
    take(q6.p2.u2)
  }
}
}

