// RUN: %target-swift-frontend %s -emit-silgen

// rdar://problem/19792768

public func foo<
    Expected : SequenceType,
    Actual : SequenceType,
    T : Comparable
    where
      Expected.Generator.Element == Actual.Generator.Element,
      Expected.Generator.Element == (T, T)
>(expected: Expected, _ actual: Actual) {}

func f() {
  foo(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    [ (10, 1010), (20, 1020), (30, 1030) ])
}

