// Key paths that capture values (subscript arguments) in Embedded Swift.
//
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O %t/ok.swift -module-name ok -c -o %t/ok-o.o
// RUN: %target-embedded-link %t/ok-o.o -o %t/ok-o.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/ok-o.out | %FileCheck %s

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -Onone %t/ok.swift -module-name ok -c -o %t/ok-onone.o
// RUN: %target-embedded-link %t/ok-onone.o -o %t/ok-onone.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/ok-onone.out | %FileCheck %s

// The performance hint fires for every capturing key path. It's emitted from a
// SIL pass, so this needs to get past type checking, and it's in the
// `PerformanceHints` group, which is ignored by default, so ask for it.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O -Wwarning PerformanceHints -emit-ir -o /dev/null -verify %t/hint.swift -module-name hint

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

//--- ok.swift

public struct Grid {
  public var cells: (Int32, Int32, Int32, Int32) = (0, 0, 0, 0)
  public subscript(i: Int) -> Int32 {
    get {
      switch i {
      case 0: return cells.0
      case 1: return cells.1
      case 2: return cells.2
      default: return cells.3
      }
    }
    set {
      switch i {
      case 0: cells.0 = newValue
      case 1: cells.1 = newValue
      case 2: cells.2 = newValue
      default: cells.3 = newValue
      }
    }
  }
  public init() {}
}

// Two captured values, of different sizes, to check the argument area's
// alignment and packing.
public struct Table {
  public var v: Int32 = 0
  public subscript(a: Int8, b: Int) -> Int32 {
    get { v &+ Int32(a) &+ Int32(b) }
    set { v = newValue &- Int32(a) &- Int32(b) }
  }
  public init() {}
}

@inline(never) public func kpAt(_ i: Int) -> WritableKeyPath<Grid, Int32> { \Grid[i] }
@inline(never) public func kpTable(_ a: Int8, _ b: Int) -> WritableKeyPath<Table, Int32> {
  \Table[a, b]
}
@inline(never) public func read<R, V>(_ r: R, _ k: KeyPath<R, V>) -> V { r[keyPath: k] }

@main
struct Main {
  static func main() {
    var g = Grid()
    g.cells = (10, 20, 30, 40)

    // Reading through a captured index.
    print(read(g, kpAt(2)) == 30 ? "OK!" : "FAIL") // CHECK: OK!

    // Writing through one.
    g[keyPath: kpAt(1)] = 99
    print(g.cells.1 == 99 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    // Siblings untouched.
    print(g.cells.0 == 10 && g.cells.2 == 30 ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // Distinct captures address distinct storage.
    g[keyPath: kpAt(3)] = 77
    print(g.cells.3 == 77 && g.cells.1 == 99 ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // Equality compares the captured values, not object identity — these are
    // two separate allocations.
    print(kpAt(2) == kpAt(2) ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    print(kpAt(2) != kpAt(3) ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // Multiple captures of differing size.
    var t = Table()
    t.v = 100
    print(read(t, kpTable(1, 2)) == 103 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    t[keyPath: kpTable(1, 2)] = 110
    print(t.v == 107 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    print(kpTable(1, 2) == kpTable(1, 2) ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    print(kpTable(1, 2) != kpTable(1, 3) ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    // Forming many of them exercises allocation and release; if `deinit` failed
    // to run the argument destroy witness this would leak, and if the instance
    // were wrongly immortal the refcount would never reach zero.
    var total: Int32 = 0
    for i in 0..<4 {
      total &+= read(g, kpAt(i))
    }
    print(total == 10 &+ 99 &+ 30 &+ 77 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
  }
}

//--- hint.swift

public struct Grid {
  public var v: Int32 = 0
  public subscript(i: Int) -> Int32 {
    get { v }
    set { v = newValue }
  }
  public init() {}
}

public func capturing(_ i: Int) -> WritableKeyPath<Grid, Int32> {
  \Grid[i] // expected-warning {{a key path that captures 1 value requires a heap allocation}}
}

// A key path with no captures must not be hinted about.
public func notCapturing() -> WritableKeyPath<Grid, Int32> {
  \Grid.v
}
