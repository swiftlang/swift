// RUN: %target-swift-frontend %s -emit-silgen

// rdar://problem/19792730

public func foo<
  Expected : Sequence,
  Actual : Sequence,
  T : Comparable
>(expected: Expected, actual: Actual)
  where
  Expected.Iterator.Element == Actual.Iterator.Element,
  Expected.Iterator.Element == T {}

public func foo<
  Expected : Sequence,
  Actual : Sequence,
  T : Comparable
>(expected: Expected, actual: Actual)
  where
  Expected.Iterator.Element == Actual.Iterator.Element,
  Expected.Iterator.Element == (T, T) {}

