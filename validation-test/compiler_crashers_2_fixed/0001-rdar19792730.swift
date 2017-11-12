// RUN: %target-swift-frontend %s -emit-silgen

// rdar://problem/19792730

public func foo<
  Expected : Sequence,
  Actual : Sequence,
  T : Comparable
  where
  Expected.Iterator.Element == Actual.Iterator.Element,
  Expected.Iterator.Element == T
>(expected: Expected, actual: Actual) {}

public func foo<
  Expected : Sequence,
  Actual : Sequence,
  T : Comparable
  where
  Expected.Iterator.Element == Actual.Iterator.Element,
  Expected.Iterator.Element == (T, T)
>(expected: Expected, actual: Actual) {}

