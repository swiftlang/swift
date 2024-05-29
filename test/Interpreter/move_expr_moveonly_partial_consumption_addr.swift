// RUN: %target-run-simple-swift(-parse-as-library -Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-parse-as-library -O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

@main struct App { static func main() {
  test1((1,2,3,4))
}}

func barrier() { print("barrier") }

struct Ur<T> : ~Copyable {
  var t: T
  var name: String
  init(_ t: T, named name: String) {
    self.t = t
    self.name = name
    print("hi", name)
  }
  deinit {
    print("bye", name)
  }
}
func take<T>(_ u: consuming Ur<T>) {}

struct Pair<T> : ~Copyable {
  var u1: Ur<T>
  var u2: Ur<T>
  init(_ t1: T, _ t2: T, named name: String) {
    u1 = .init(t1, named: "\(name).u1")
    u2 = .init(t2, named: "\(name).u2")
  }
}
func take<T>(_ u: consuming Pair<T>) {}

struct Quad<T> : ~Copyable {
  var p1: Pair<T>
  var p2: Pair<T>
  init(_ t1: T, _ t2: T, _ t3: T, _ t4: T, named name: String) {
    p1 = .init(t1, t2, named: "\(name).p1")
    p2 = .init(t3, t4, named: "\(name).p2")
  }
}
func take<T>(_ u: consuming Quad<T>) {}

func test1<T>(_ ts: (T, T, T, T)) {
do {
  let q1 = Quad<T>(ts.0, ts.1, ts.2, ts.3, named: "\(#function).q1")
  barrier()
  // CHECK: barrier
  // CHECK: bye test1(_:).q1.p1.u1
  // CHECK: bye test1(_:).q1.p1.u2
  // CHECK: bye test1(_:).q1.p2.u1
  // CHECK: bye test1(_:).q1.p2.u2
}

  let q2 = Quad<T>(ts.0, ts.1, ts.2, ts.3, named: "\(#function).q2")
  take(q2.p2.u2)
  take(q2.p2.u1)
  barrier()
  take(q2.p1.u2)
  take(q2.p1.u1)
  // CHECK: bye test1(_:).q2.p2.u2
  // CHECK: bye test1(_:).q2.p2.u1
  // CHECK: barrier
  // CHECK: bye test1(_:).q2.p1.u2
  // CHECK: bye test1(_:).q2.p1.u1

  let q3 = Quad<T>(ts.0, ts.1, ts.2, ts.3, named: "\(#function).q3")
  _ = consume q3.p2.u2
  _ = consume q3.p2.u1
  _ = consume q3.p1.u2
  barrier()
  _ = consume q3.p1.u1
  // CHECK: bye test1(_:).q3.p2.u2
  // CHECK: bye test1(_:).q3.p2.u1
  // CHECK: bye test1(_:).q3.p1.u2
  // CHECK: barrier
  // CHECK: bye test1(_:).q3.p1.u1

  let q4 = Quad<T>(ts.0, ts.1, ts.2, ts.3, named: "\(#function).q4")
  _ = consume q4.p1.u1
  barrier()
  _ = consume q4.p2.u1
  _ = consume q4.p1.u2
  _ = consume q4.p2.u2
  // CHECK: bye test1(_:).q4.p1.u1
  // CHECK: barrier
  // CHECK: bye test1(_:).q4.p2.u1
  // CHECK: bye test1(_:).q4.p1.u2
  // CHECK: bye test1(_:).q4.p2.u2

do {
  let q5 = Quad<T>(ts.0, ts.1, ts.2, ts.3, named: "\(#function).q5")
  take(q5.p1.u1)
  _ = consume q5.p2.u1
  _ = consume q5.p1.u2
  take(q5.p2.u2)
  // CHECK: bye test1(_:).q5.p1.u1
  // CHECK: bye test1(_:).q5.p2.u1
  // CHECK: bye test1(_:).q5.p1.u2
  // CHECK: bye test1(_:).q5.p2.u2
}
do {
  let q6 = Quad<T>(ts.0, ts.1, ts.2, ts.3, named: "\(#function).q6")
  if Bool.random() {
    take(q6.p1.u1)
    _ = consume q6.p2.u1
  } else {
    _ = consume q6.p1.u2
    take(q6.p2.u2)
  }
}
}

