// RUN: %target-typecheck-verify-swift -solver-scope-threshold=2000
// REQUIRES: tools-release,no_asan
// REQUIRES: OS=macosx

// This restates the test in rdar36838495.swift, but insulates it from
// changes in the standard library.
// Note: at time of original commit, works with threshold of 1908

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
let _ = x.values.filterRT { $0.isX }
                .filterRT { $0.t.b != .a }
                .filterRT { $0.c == .a || $0.c == .b }
                .filterRT { $0.isX }
                .filterRT { $0.t.b != .a }
                .sorted { $0.name < $1.name }

extension Sequence {
  func filterRT(_ predicate: (Element) throws -> Bool) rethrows -> [Element] {
    fatalError()
  }
}

extension RangeReplaceableCollection {
  func filterRT(_ predicate: (Element) throws -> Bool) rethrows -> Self {
    fatalError()
  }
}

extension Array {
  func filterRT(_ predicate: (Element) throws -> Bool) rethrows -> [Element] {
    fatalError()
  }
}
