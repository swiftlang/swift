// RUN: %target-swift-frontend %s -emit-silgen

// rdar://problem/19792768

public func foo<
  Expected : Sequence,
  Actual : Sequence,
  T : Comparable
  where
  Expected.Iterator.Element == Actual.Iterator.Element,
  Expected.Iterator.Element == (T, T)
>(_ expected: Expected, _ actual: Actual) {}

func f() {
  foo(
    [ (10, 1010), (20, 1020), (30, 1030) ],
    [ (10, 1010), (20, 1020), (30, 1030) ])
}

