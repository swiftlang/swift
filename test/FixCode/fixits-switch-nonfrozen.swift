// RUN: not %target-swift-frontend -emit-sil %s -emit-fixits-path %t.remap -enable-resilience -enable-nonfrozen-enum-exhaustivity-diagnostics -diagnostics-editor-mode
// RUN: c-arcmt-test %t.remap | arcmt-test -verify-transformed-files %s.result

enum Runcible {
  case spoon
  case hat
  case fork
}

func checkDiagnosticMinimality(x: Runcible?) {
  switch (x!, x!) {
  case (.spoon, .spoon):
    break
  case (.spoon, .hat):
    break
  case (.hat, .spoon):
    break
  }

  switch (x!, x!) {
  case (.spoon, .spoon):
    break
  case (.hat, .hat):
    break
  }
}

enum LargeSpaceEnum {
  case case0
  case case1
  case case2
  case case3
  case case4
  case case5
  case case6
  case case7
  case case8
  case case9
  case case10
}

func notQuiteBigEnough() -> Bool {
  switch (LargeSpaceEnum.case1, LargeSpaceEnum.case2) {
  case (.case0, .case0): return true
  case (.case1, .case1): return true
  case (.case2, .case2): return true
  case (.case3, .case3): return true
  case (.case4, .case4): return true
  case (.case5, .case5): return true
  case (.case6, .case6): return true
  case (.case7, .case7): return true
  case (.case8, .case8): return true
  case (.case9, .case9): return true
  case (.case10, .case10): return true
  }
}

enum OverlyLargeSpaceEnum {
  case case0
  case case1
  case case2
  case case3
  case case4
  case case5
  case case6
  case case7
  case case8
  case case9
  case case10
  case case11
}

enum ContainsOverlyLargeEnum {
  case one(OverlyLargeSpaceEnum)
  case two(OverlyLargeSpaceEnum)
  case three(OverlyLargeSpaceEnum, OverlyLargeSpaceEnum)
}

func quiteBigEnough() -> Bool {
  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) {
  case (.case0, .case0): return true
  case (.case1, .case1): return true
  case (.case2, .case2): return true
  case (.case3, .case3): return true
  case (.case4, .case4): return true
  case (.case5, .case5): return true
  case (.case6, .case6): return true
  case (.case7, .case7): return true
  case (.case8, .case8): return true
  case (.case9, .case9): return true
  case (.case10, .case10): return true
  case (.case11, .case11): return true
  }

  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) {
  case (.case0, _): return true
  case (.case1, _): return true
  case (.case2, _): return true
  case (.case3, _): return true
  case (.case4, _): return true
  case (.case5, _): return true
  case (.case6, _): return true
  case (.case7, _): return true
  case (.case8, _): return true
  case (.case9, _): return true
  case (.case10, _): return true
  }
}

indirect enum InfinitelySized {
  case one
  case two
  case recur(InfinitelySized)
  case mutualRecur(MutuallyRecursive, InfinitelySized)
}

indirect enum MutuallyRecursive {
  case one
  case two
  case recur(MutuallyRecursive)
  case mutualRecur(InfinitelySized, MutuallyRecursive)
}

func infinitelySized() -> Bool {
  switch (InfinitelySized.one, InfinitelySized.one) {
  case (.one, .one): return true
  case (.two, .two): return true
  }
  
  switch (MutuallyRecursive.one, MutuallyRecursive.one) {
  case (.one, .one): return true
  case (.two, .two): return true
  }
}

public enum NonExhaustive {
  case a, b
}

public enum NonExhaustivePayload {
  case a(Int), b(Bool)
}

@_frozen public enum TemporalProxy {
  case seconds(Int)
  case milliseconds(Int)
  case microseconds(Int)
  case nanoseconds(Int)
  @_downgrade_exhaustivity_check
  case never
}

// Inlineable code is considered "outside" the module and must include a default
// case.
@inlinable
public func testNonExhaustive(_ value: NonExhaustive, _ payload: NonExhaustivePayload, for interval: TemporalProxy, flag: Bool) {
  switch value {
  case .a: break
  }

  switch value {
  case .a: break
  case .b: break
  }
  
  switch value {
  case .a: break
  case .b: break
  default: break // no-warning
  }

  switch value {
  case .a: break
  case .b: break
  @unknown case _: break // no-warning
  }

  switch value {
  case .a: break
  @unknown case _: break
  }

  switch value {
  @unknown case _: break
  }

  // Test being part of other spaces.
  switch value as Optional {
  case .a?: break
  case .b?: break
  case nil: break
  }

  switch value as Optional {
  case .a?: break
  case .b?: break
  case nil: break
  @unknown case _: break
  } // no-warning

  switch value as Optional {
  case _?: break
  case nil: break
  } // no-warning

  switch (value, flag) {
  case (.a, _): break
  case (.b, false): break
  case (_, true): break
  }

  switch (value, flag) {
  case (.a, _): break
  case (.b, false): break
  case (_, true): break
  @unknown case _: break
  } // no-warning

  switch (flag, value) {
  case (_, .a): break
  case (false, .b): break
  case (true, _): break
  }

  switch (flag, value) {
  case (_, .a): break
  case (false, .b): break
  case (true, _): break
  @unknown case _: break
  } // no-warning

  switch (value, value) {
  case (.a, _), (_, .a): break
  case (.b, _), (_, .b): break
  }

  switch (value, value) {
  case (.a, _), (_, .a): break
  case (.b, _), (_, .b): break
  @unknown case _: break
  } // no-warning

  // Test interaction with @_downgrade_exhaustivity_check.
  switch (value, interval) {
  case (_, .seconds): break
  case (.a, _): break
  case (.b, _): break
  }

  switch (value, interval) {
  case (_, .never): break
  case (.a, _): break
  case (.b, _): break
  }

  // Test payloaded enums.
  switch payload {
  case .a: break
  }

  switch payload {
  case .a: break
  case .b: break
  }
  
  switch payload {
  case .a: break
  case .b: break
  default: break // no-warning
  }

  switch payload {
  case .a: break
  case .b: break
  @unknown case _: break // no-warning
  }

  switch payload {
  case .a: break
  @unknown case _: break
  }

  switch payload {
  case .a: break
  case .b(false): break
  }

  switch payload {
  case .a: break
  case .b(false): break
  @unknown case _: break
  }
}
