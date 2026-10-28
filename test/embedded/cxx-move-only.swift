// RUN: %empty-directory(%t)
// RUN: %target-clang -c %S/Inputs/cxx-move-only-counts.cpp -o %t/counts.o
// RUN: %target-swift-frontend -I %S/Inputs %s -enable-experimental-feature Embedded -enable-experimental-feature Extern -enable-experimental-feature MoveOnlyTuples -cxx-interoperability-mode=default -wmo -O -parse-as-library -c -o %t/main.o
// RUN: %target-clang %target-clang-resource-dir-opt %t/main.o %t/counts.o -lc++ %target-embedded-posix-shim -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Extern
// REQUIRES: swift_feature_MoveOnlyTuples

import CxxMoveOnly

@_extern(c) func reportCounts()

struct Box: ~Copyable {
  var m: MoveOnly
  init() { m = MoveOnly() }
}

struct BoxWithDeinit: ~Copyable {
  var m: MoveOnly
  init() { m = MoveOnly() }
  deinit {}
}

struct Wrapper<T: ~Copyable>: ~Copyable {
  var v: T
  init(_ v: consuming T) { self.v = v }
}

struct WrapperWithDeinit<T: ~Copyable>: ~Copyable {
  var v: T
  init(_ v: consuming T) { self.v = v }
  deinit {}
}

enum MaybeMoveOnly: ~Copyable {
  case none
  case some(MoveOnly)
}

final class Holder {
  var m: MoveOnly
  init() { m = MoveOnly() }
}

@inline(never)
func eat<T: ~Copyable>(_ x: consuming T) {}

@main
struct Main {
  static func main() {
    do {
      let m = MoveOnly()
      precondition(m.value() == 7)
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    do {
      let b = Box()
      precondition(b.m.value() == 7)
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    do {
      let b = BoxWithDeinit()
      precondition(b.m.value() == 7)
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    do {
      let w = Wrapper(MoveOnly())
      precondition(w.v.value() == 7)
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    do {
      let t = (MoveOnly(), 1)
      precondition(t.1 == 1)
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    do {
      let w = WrapperWithDeinit(MoveOnly())
      precondition(w.v.value() == 7)
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    do {
      let e = MaybeMoveOnly.some(MoveOnly())
      if case .some(let m) = e { precondition(m.value() == 7) }
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    do {
      let e = MaybeMoveOnly.none
      if case .none = e {}
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    do {
      let h = Holder()
      precondition(h.m.value() == 7)
    }
    reportCounts()
    // CHECK: balanced=yes doubleFree=0

    eat(MoveOnly())
    reportCounts()
    // CHECK: balanced=yes doubleFree=0
  }
}
