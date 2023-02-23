// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan
// REQUIRES: OS=macosx

struct T {
  enum B {
    case a
    case b
  }

  let name: String
  let b: B
}

struct A {
  enum C {
    case a
    case b
    case c
  }

  let name: String
  let t: T
  let c: C

  var isX: Bool {
    return self.t.b == .a
  }
}

let x: [String: A] = [:]
let _ = x.values.filter { $0.isX }
                .filter { $0.t.b != .a }
                .filter { $0.c == .a || $0.c == .b }
                .filter { $0.isX }
                .filter { $0.t.b != .a }
                .sorted { $0.name < $1.name }
