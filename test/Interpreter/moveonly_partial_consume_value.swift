// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all) | %FileCheck %s

// REQUIRES: executable_test

func destructure() {
  let q = getQuad(name: "q")

  // CHECK: hi q.p1.u1
  // CHECK: hi q.p1.u2
  // CHECK: hi q.p2.u1
  // CHECK: hi q.p2.u2

  take(q.p1.u1)
  // CHECK: bye q.p1.u1
  take(q.p1.u2)
  // CHECK: bye q.p1.u2
  take(q.p2.u1)
  // CHECK: bye q.p2.u1
  take(q.p2.u2)
  // CHECK: bye q.p2.u2
}

struct Unique : ~Copyable {
  let name: String
  init(name: String) {
    self.name = name
    print("hi", name)
  }
  deinit {
    print("bye", name)
  }
}

func take(_ u: consuming Unique) {}

struct Pair : ~Copyable {
  var u1: Unique
  var u2: Unique
  init(name: String) {
    self.u1 = .init(name: "\(name).u1")
    self.u2 = .init(name: "\(name).u2")
  }
}

struct Quad : ~Copyable {
  var p1: Pair
  var p2: Pair
  init(name: String) {
    self.p1 = .init(name: "\(name).p1")
    self.p2 = .init(name: "\(name).p2")
  }
}

func getQuad(name: String) -> Quad {
  return Quad(name: name)
}

destructure()
