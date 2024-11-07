// RUN: %target-swift-frontend -emit-sil %s -enable-library-evolution -enable-nonfrozen-enum-exhaustivity-diagnostics -verify -swift-version 6

enum Runcible {
  case spoon
  case hat
  case fork
}

func checkDiagnosticMinimality(x: Runcible?) {
  switch (x!, x!) {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '(.fork, _)'}} {{+12:3-3=case (.fork, _):\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '(.hat, .hat)'}} {{+12:3-3=case (.hat, .hat):\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '(_, .fork)'}} {{+12:3-3=case (_, .fork):\n<#code#>\n}}
  // expected-note@-5 {{add missing cases}} {{+12:3-3=case (.fork, _):\n<#code#>\ncase (.hat, .hat):\n<#code#>\ncase (_, .fork):\n<#code#>\n}}
  case (.spoon, .spoon):
    break
  case (.spoon, .hat):
    break
  case (.hat, .spoon):
    break
  }

  switch (x!, x!) {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '(.fork, _)'}} {{+11:3-3=case (.fork, _):\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '(.hat, .spoon)'}} {{+11:3-3=case (.hat, .spoon):\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '(.spoon, .hat)'}} {{+11:3-3=case (.spoon, .hat):\n<#code#>\n}}
  // expected-note@-5 {{add missing case: '(_, .fork)'}} {{+11:3-3=case (_, .fork):\n<#code#>\n}}
  // expected-note@-6 {{add missing cases}} {{+11:3-3=case (.fork, _):\n<#code#>\ncase (.hat, .spoon):\n<#code#>\ncase (.spoon, .hat):\n<#code#>\ncase (_, .fork):\n<#code#>\n}} 
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
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 110{{add missing case}}
  // expected-note@-3 {{add missing cases}} {{+15:3-3=case (.case10, .case0):\n<#code#>\ncase (.case10, .case1):\n<#code#>\ncase (.case10, .case2):\n<#code#>\ncase (.case10, .case3):\n<#code#>\ncase (.case10, .case4):\n<#code#>\ncase (.case10, .case5):\n<#code#>\ncase (.case10, .case6):\n<#code#>\ncase (.case10, .case7):\n<#code#>\ncase (.case10, .case8):\n<#code#>\ncase (.case10, .case9):\n<#code#>\ncase (.case9, .case0):\n<#code#>\ncase (.case9, .case1):\n<#code#>\ncase (.case9, .case2):\n<#code#>\ncase (.case9, .case3):\n<#code#>\ncase (.case9, .case4):\n<#code#>\ncase (.case9, .case5):\n<#code#>\ncase (.case9, .case6):\n<#code#>\ncase (.case9, .case7):\n<#code#>\ncase (.case9, .case8):\n<#code#>\ncase (.case9, .case10):\n<#code#>\ncase (.case8, .case0):\n<#code#>\ncase (.case8, .case1):\n<#code#>\ncase (.case8, .case2):\n<#code#>\ncase (.case8, .case3):\n<#code#>\ncase (.case8, .case4):\n<#code#>\ncase (.case8, .case5):\n<#code#>\ncase (.case8, .case6):\n<#code#>\ncase (.case8, .case7):\n<#code#>\ncase (.case8, .case9):\n<#code#>\ncase (.case8, .case10):\n<#code#>\ncase (.case7, .case0):\n<#code#>\ncase (.case7, .case1):\n<#code#>\ncase (.case7, .case2):\n<#code#>\ncase (.case7, .case3):\n<#code#>\ncase (.case7, .case4):\n<#code#>\ncase (.case7, .case5):\n<#code#>\ncase (.case7, .case6):\n<#code#>\ncase (.case7, .case8):\n<#code#>\ncase (.case7, .case9):\n<#code#>\ncase (.case7, .case10):\n<#code#>\ncase (.case6, .case0):\n<#code#>\ncase (.case6, .case1):\n<#code#>\ncase (.case6, .case2):\n<#code#>\ncase (.case6, .case3):\n<#code#>\ncase (.case6, .case4):\n<#code#>\ncase (.case6, .case5):\n<#code#>\ncase (.case6, .case7):\n<#code#>\ncase (.case6, .case8):\n<#code#>\ncase (.case6, .case9):\n<#code#>\ncase (.case6, .case10):\n<#code#>\ncase (.case5, .case0):\n<#code#>\ncase (.case5, .case1):\n<#code#>\ncase (.case5, .case2):\n<#code#>\ncase (.case5, .case3):\n<#code#>\ncase (.case5, .case4):\n<#code#>\ncase (.case5, .case6):\n<#code#>\ncase (.case5, .case7):\n<#code#>\ncase (.case5, .case8):\n<#code#>\ncase (.case5, .case9):\n<#code#>\ncase (.case5, .case10):\n<#code#>\ncase (.case4, .case0):\n<#code#>\ncase (.case4, .case1):\n<#code#>\ncase (.case4, .case2):\n<#code#>\ncase (.case4, .case3):\n<#code#>\ncase (.case4, .case5):\n<#code#>\ncase (.case4, .case6):\n<#code#>\ncase (.case4, .case7):\n<#code#>\ncase (.case4, .case8):\n<#code#>\ncase (.case4, .case9):\n<#code#>\ncase (.case4, .case10):\n<#code#>\ncase (.case3, .case0):\n<#code#>\ncase (.case3, .case1):\n<#code#>\ncase (.case3, .case2):\n<#code#>\ncase (.case3, .case4):\n<#code#>\ncase (.case3, .case5):\n<#code#>\ncase (.case3, .case6):\n<#code#>\ncase (.case3, .case7):\n<#code#>\ncase (.case3, .case8):\n<#code#>\ncase (.case3, .case9):\n<#code#>\ncase (.case3, .case10):\n<#code#>\ncase (.case2, .case0):\n<#code#>\ncase (.case2, .case1):\n<#code#>\ncase (.case2, .case3):\n<#code#>\ncase (.case2, .case4):\n<#code#>\ncase (.case2, .case5):\n<#code#>\ncase (.case2, .case6):\n<#code#>\ncase (.case2, .case7):\n<#code#>\ncase (.case2, .case8):\n<#code#>\ncase (.case2, .case9):\n<#code#>\ncase (.case2, .case10):\n<#code#>\ncase (.case1, .case0):\n<#code#>\ncase (.case1, .case2):\n<#code#>\ncase (.case1, .case3):\n<#code#>\ncase (.case1, .case4):\n<#code#>\ncase (.case1, .case5):\n<#code#>\ncase (.case1, .case6):\n<#code#>\ncase (.case1, .case7):\n<#code#>\ncase (.case1, .case8):\n<#code#>\ncase (.case1, .case9):\n<#code#>\ncase (.case1, .case10):\n<#code#>\ncase (.case0, .case1):\n<#code#>\ncase (.case0, .case2):\n<#code#>\ncase (.case0, .case3):\n<#code#>\ncase (.case0, .case4):\n<#code#>\ncase (.case0, .case5):\n<#code#>\ncase (.case0, .case6):\n<#code#>\ncase (.case0, .case7):\n<#code#>\ncase (.case0, .case8):\n<#code#>\ncase (.case0, .case9):\n<#code#>\ncase (.case0, .case10):\n<#code#>\n}} 
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
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 132{{add missing case}}
  // expected-note@-3 {{add missing cases}} {{+16:3-3=case (.case11, .case0):\n<#code#>\ncase (.case11, .case1):\n<#code#>\ncase (.case11, .case2):\n<#code#>\ncase (.case11, .case3):\n<#code#>\ncase (.case11, .case4):\n<#code#>\ncase (.case11, .case5):\n<#code#>\ncase (.case11, .case6):\n<#code#>\ncase (.case11, .case7):\n<#code#>\ncase (.case11, .case8):\n<#code#>\ncase (.case11, .case9):\n<#code#>\ncase (.case11, .case10):\n<#code#>\ncase (.case10, .case0):\n<#code#>\ncase (.case10, .case1):\n<#code#>\ncase (.case10, .case2):\n<#code#>\ncase (.case10, .case3):\n<#code#>\ncase (.case10, .case4):\n<#code#>\ncase (.case10, .case5):\n<#code#>\ncase (.case10, .case6):\n<#code#>\ncase (.case10, .case7):\n<#code#>\ncase (.case10, .case8):\n<#code#>\ncase (.case10, .case9):\n<#code#>\ncase (.case10, .case11):\n<#code#>\ncase (.case9, .case0):\n<#code#>\ncase (.case9, .case1):\n<#code#>\ncase (.case9, .case2):\n<#code#>\ncase (.case9, .case3):\n<#code#>\ncase (.case9, .case4):\n<#code#>\ncase (.case9, .case5):\n<#code#>\ncase (.case9, .case6):\n<#code#>\ncase (.case9, .case7):\n<#code#>\ncase (.case9, .case8):\n<#code#>\ncase (.case9, .case10):\n<#code#>\ncase (.case9, .case11):\n<#code#>\ncase (.case8, .case0):\n<#code#>\ncase (.case8, .case1):\n<#code#>\ncase (.case8, .case2):\n<#code#>\ncase (.case8, .case3):\n<#code#>\ncase (.case8, .case4):\n<#code#>\ncase (.case8, .case5):\n<#code#>\ncase (.case8, .case6):\n<#code#>\ncase (.case8, .case7):\n<#code#>\ncase (.case8, .case9):\n<#code#>\ncase (.case8, .case10):\n<#code#>\ncase (.case8, .case11):\n<#code#>\ncase (.case7, .case0):\n<#code#>\ncase (.case7, .case1):\n<#code#>\ncase (.case7, .case2):\n<#code#>\ncase (.case7, .case3):\n<#code#>\ncase (.case7, .case4):\n<#code#>\ncase (.case7, .case5):\n<#code#>\ncase (.case7, .case6):\n<#code#>\ncase (.case7, .case8):\n<#code#>\ncase (.case7, .case9):\n<#code#>\ncase (.case7, .case10):\n<#code#>\ncase (.case7, .case11):\n<#code#>\ncase (.case6, .case0):\n<#code#>\ncase (.case6, .case1):\n<#code#>\ncase (.case6, .case2):\n<#code#>\ncase (.case6, .case3):\n<#code#>\ncase (.case6, .case4):\n<#code#>\ncase (.case6, .case5):\n<#code#>\ncase (.case6, .case7):\n<#code#>\ncase (.case6, .case8):\n<#code#>\ncase (.case6, .case9):\n<#code#>\ncase (.case6, .case10):\n<#code#>\ncase (.case6, .case11):\n<#code#>\ncase (.case5, .case0):\n<#code#>\ncase (.case5, .case1):\n<#code#>\ncase (.case5, .case2):\n<#code#>\ncase (.case5, .case3):\n<#code#>\ncase (.case5, .case4):\n<#code#>\ncase (.case5, .case6):\n<#code#>\ncase (.case5, .case7):\n<#code#>\ncase (.case5, .case8):\n<#code#>\ncase (.case5, .case9):\n<#code#>\ncase (.case5, .case10):\n<#code#>\ncase (.case5, .case11):\n<#code#>\ncase (.case4, .case0):\n<#code#>\ncase (.case4, .case1):\n<#code#>\ncase (.case4, .case2):\n<#code#>\ncase (.case4, .case3):\n<#code#>\ncase (.case4, .case5):\n<#code#>\ncase (.case4, .case6):\n<#code#>\ncase (.case4, .case7):\n<#code#>\ncase (.case4, .case8):\n<#code#>\ncase (.case4, .case9):\n<#code#>\ncase (.case4, .case10):\n<#code#>\ncase (.case4, .case11):\n<#code#>\ncase (.case3, .case0):\n<#code#>\ncase (.case3, .case1):\n<#code#>\ncase (.case3, .case2):\n<#code#>\ncase (.case3, .case4):\n<#code#>\ncase (.case3, .case5):\n<#code#>\ncase (.case3, .case6):\n<#code#>\ncase (.case3, .case7):\n<#code#>\ncase (.case3, .case8):\n<#code#>\ncase (.case3, .case9):\n<#code#>\ncase (.case3, .case10):\n<#code#>\ncase (.case3, .case11):\n<#code#>\ncase (.case2, .case0):\n<#code#>\ncase (.case2, .case1):\n<#code#>\ncase (.case2, .case3):\n<#code#>\ncase (.case2, .case4):\n<#code#>\ncase (.case2, .case5):\n<#code#>\ncase (.case2, .case6):\n<#code#>\ncase (.case2, .case7):\n<#code#>\ncase (.case2, .case8):\n<#code#>\ncase (.case2, .case9):\n<#code#>\ncase (.case2, .case10):\n<#code#>\ncase (.case2, .case11):\n<#code#>\ncase (.case1, .case0):\n<#code#>\ncase (.case1, .case2):\n<#code#>\ncase (.case1, .case3):\n<#code#>\ncase (.case1, .case4):\n<#code#>\ncase (.case1, .case5):\n<#code#>\ncase (.case1, .case6):\n<#code#>\ncase (.case1, .case7):\n<#code#>\ncase (.case1, .case8):\n<#code#>\ncase (.case1, .case9):\n<#code#>\ncase (.case1, .case10):\n<#code#>\ncase (.case1, .case11):\n<#code#>\ncase (.case0, .case1):\n<#code#>\ncase (.case0, .case2):\n<#code#>\ncase (.case0, .case3):\n<#code#>\ncase (.case0, .case4):\n<#code#>\ncase (.case0, .case5):\n<#code#>\ncase (.case0, .case6):\n<#code#>\ncase (.case0, .case7):\n<#code#>\ncase (.case0, .case8):\n<#code#>\ncase (.case0, .case9):\n<#code#>\ncase (.case0, .case10):\n<#code#>\ncase (.case0, .case11):\n<#code#>\n}}
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
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '(.case11, _)'}} {{+14:3-3=case (.case11, _):\n<#code#>\n}}
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
// expected-error@-1 {{switch must be exhaustive}}
// expected-note@-2 {{add missing case: '(.recur(_), _)'}} {{+13:3-3=case (.recur(_), _):\n<#code#>\n}}
// expected-note@-3 {{add missing case: '(.mutualRecur(_, _), _)'}} {{+13:3-3=case (.mutualRecur(_, _), _):\n<#code#>\n}}
// expected-note@-4 {{add missing case: '(.two, .one)'}} {{+13:3-3=case (.two, .one):\n<#code#>\n}}
// expected-note@-5 {{add missing case: '(.recur(_), .mutualRecur(_, _))'}} {{+13:3-3=case (.recur(_), .mutualRecur(_, _)):\n<#code#>\n}}
// expected-note@-6 {{add missing case: '(.mutualRecur(_, _), .mutualRecur(_, _))'}} {{+13:3-3=case (.mutualRecur(_, _), .mutualRecur(_, _)):\n<#code#>\n}}
// expected-note@-7 {{add missing case: '(.one, .two)'}} {{+13:3-3=case (.one, .two):\n<#code#>\n}}
// expected-note@-8 {{add missing case: '(_, .recur(_))'}} {{+13:3-3=case (_, .recur(_)):\n<#code#>\n}}
// expected-note@-9 {{add missing case: '(_, .mutualRecur(_, _))'}} {{+13:3-3=case (_, .mutualRecur(_, _)):\n<#code#>\n}}
// expected-note@-10 {{add missing cases}} {{+13:3-3=case (.recur(_), _):\n<#code#>\ncase (.mutualRecur(_, _), _):\n<#code#>\ncase (.two, .one):\n<#code#>\ncase (.recur(_), .mutualRecur(_, _)):\n<#code#>\ncase (.mutualRecur(_, _), .mutualRecur(_, _)):\n<#code#>\ncase (.one, .two):\n<#code#>\ncase (_, .recur(_)):\n<#code#>\ncase (_, .mutualRecur(_, _)):\n<#code#>\n}} 
  case (.one, .one): return true
  case (.two, .two): return true
  }
  
  switch (MutuallyRecursive.one, MutuallyRecursive.one) {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '(.recur(_), _)'}} {{+13:3-3=case (.recur(_), _):\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '(.mutualRecur(_, _), _)'}} {{+13:3-3=case (.mutualRecur(_, _), _):\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '(.two, .one)'}} {{+13:3-3=case (.two, .one):\n<#code#>\n}}
  // expected-note@-5 {{add missing case: '(.recur(_), .mutualRecur(_, _))'}} {{+13:3-3=case (.recur(_), .mutualRecur(_, _)):\n<#code#>\n}}
  // expected-note@-6 {{add missing case: '(.mutualRecur(_, _), .mutualRecur(_, _))'}} {{+13:3-3=case (.mutualRecur(_, _), .mutualRecur(_, _)):\n<#code#>\n}}
  // expected-note@-7 {{add missing case: '(.one, .two)'}} {{+13:3-3=case (.one, .two):\n<#code#>\n}}
  // expected-note@-8 {{add missing case: '(_, .recur(_))'}} {{+13:3-3=case (_, .recur(_)):\n<#code#>\n}}
  // expected-note@-9 {{add missing case: '(_, .mutualRecur(_, _))'}} {{+13:3-3=case (_, .mutualRecur(_, _)):\n<#code#>\n}}
  // expected-note@-10 {{add missing cases}} {{+13:3-3=case (.recur(_), _):\n<#code#>\ncase (.mutualRecur(_, _), _):\n<#code#>\ncase (.two, .one):\n<#code#>\ncase (.recur(_), .mutualRecur(_, _)):\n<#code#>\ncase (.mutualRecur(_, _), .mutualRecur(_, _)):\n<#code#>\ncase (.one, .two):\n<#code#>\ncase (_, .recur(_)):\n<#code#>\ncase (_, .mutualRecur(_, _)):\n<#code#>\n}} 
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

@frozen public enum TemporalProxy {
  case seconds(Int)
  case milliseconds(Int)
  case microseconds(Int)
  case nanoseconds(Int)
  case never
}

// Inlineable code is considered "outside" the module and must include a default
// case.
@inlinable
public func testNonExhaustive(_ value: NonExhaustive, _ payload: NonExhaustivePayload, for interval: TemporalProxy, flag: Bool) {
  switch value {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.b'}} {{+6:3-3=case .b:\n<#code#>\n}}
  // expected-note@-3 {{handle unknown values using "@unknown default"}} {{+6:3-3=@unknown default:\n<#fatalError()#>\n}}
  // expected-note@-4 {{add missing cases}} {{+6:3-3=case .b:\n<#code#>\n@unknown default:\n<#fatalError()#>\n}}
  case .a: break
  }

  switch value {
  // expected-error@-1 {{switch covers known cases, but 'NonExhaustive' may have additional unknown values}}
  // expected-note@-2 {{handle unknown values using "@unknown default"}} {{+5:3-3=@unknown default:\n<#fatalError()#>\n}}
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
  // expected-warning@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.b'}} {{+4:3-3=case .b:\n<#code#>\n}}
  case .a: break
  @unknown case _: break
  }

  switch value {
  // expected-warning@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.a'}} {{+5:3-3=case .a:\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '.b'}} {{+5:3-3=case .b:\n<#code#>\n}}
  // expected-note@-4 {{add missing cases}} {{+5:3-3=case .a:\n<#code#>\ncase .b:\n<#code#>\n}}
  @unknown case _: break
  }

  // Test being part of other spaces.
  switch value as Optional {
  // expected-error@-1 {{switch covers known cases, but 'Optional<NonExhaustive>' may have additional unknown values}}
  // expected-note@-2 {{add missing case: '.some(_)'}} {{+6:3-3=case .some(_):\n<#code#>\n}}
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
  // expected-error@-1 {{switch covers known cases, but '(NonExhaustive, Bool)' may have additional unknown values}}
  // expected-note@-2 {{add missing case: '(_, false)'}} {{+6:3-3=case (_, false):\n<#code#>\n}}
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
  // expected-error@-1 {{switch covers known cases, but '(Bool, NonExhaustive)' may have additional unknown values}}
  // expected-note@-2 {{add missing case: '(false, _)'}} {{+6:3-3=case (false, _):\n<#code#>\n}}
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
  // expected-error@-1 {{switch covers known cases, but '(NonExhaustive, NonExhaustive)' may have additional unknown values}}
  // expected-note@-2 {{add missing case: '(_, _)'}} {{+5:3-3=case (_, _):\n<#code#>\n}}
  case (.a, _), (_, .a): break
  case (.b, _), (_, .b): break
  }

  switch (value, value) {
  case (.a, _), (_, .a): break
  case (.b, _), (_, .b): break
  @unknown case _: break
  } // no-warning

  switch (value, interval) {
  // expected-error@-1 {{switch covers known cases, but '(NonExhaustive, TemporalProxy)' may have additional unknown values}}
  // expected-note@-2 {{add missing case: '(_, .milliseconds(_))'}} {{+10:3-3=case (_, .milliseconds(_)):\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '(_, .microseconds(_))'}} {{+10:3-3=case (_, .microseconds(_)):\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '(_, .nanoseconds(_))'}} {{+10:3-3=case (_, .nanoseconds(_)):\n<#code#>\n}}
  // expected-note@-5 {{add missing case: '(_, .never)'}} {{+10:3-3=case (_, .never):\n<#code#>\n}}
  // expected-note@-6 {{add missing cases}} {{+10:3-3=case (_, .milliseconds(_)):\n<#code#>\ncase (_, .microseconds(_)):\n<#code#>\ncase (_, .nanoseconds(_)):\n<#code#>\ncase (_, .never):\n<#code#>\n}} 
  case (_, .seconds): break
  case (.a, _): break
  case (.b, _): break
  }

  switch (value, interval) {
  // expected-error@-1 {{switch covers known cases, but '(NonExhaustive, TemporalProxy)' may have additional unknown values}}
  // expected-note@-2 {{add missing case: '(_, .seconds(_))'}} {{+10:3-3=case (_, .seconds(_)):\n<#code#>\n}}
  // expected-note@-3 {{add missing case: '(_, .milliseconds(_))'}} {{+10:3-3=case (_, .milliseconds(_)):\n<#code#>\n}}
  // expected-note@-4 {{add missing case: '(_, .microseconds(_))'}} {{+10:3-3=case (_, .microseconds(_)):\n<#code#>\n}}
  // expected-note@-5 {{add missing case: '(_, .nanoseconds(_))'}} {{+10:3-3=case (_, .nanoseconds(_)):\n<#code#>\n}}
  // expected-note@-6 {{add missing cases}} {{+10:3-3=case (_, .seconds(_)):\n<#code#>\ncase (_, .milliseconds(_)):\n<#code#>\ncase (_, .microseconds(_)):\n<#code#>\ncase (_, .nanoseconds(_)):\n<#code#>\n}} 
  case (_, .never): break
  case (.a, _): break
  case (.b, _): break
  }

  // Test payloaded enums.
  switch payload {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.b(_)'}} {{+6:3-3=case .b(_):\n<#code#>\n}}
  // expected-note@-3 {{handle unknown values using "@unknown default"}} {{+6:3-3=@unknown default:\n<#fatalError()#>\n}}
  // expected-note@-4 {{add missing cases}} {{+6:3-3=case .b(_):\n<#code#>\n@unknown default:\n<#fatalError()#>\n}}
  case .a: break
  }

  switch payload {
  // expected-error@-1 {{switch covers known cases, but 'NonExhaustivePayload' may have additional unknown values}}
  // expected-note@-2 {{handle unknown values using "@unknown default"}} {{+5:3-3=@unknown default:\n<#fatalError()#>\n}}
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
  // expected-warning@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.b(_)'}} {{+4:3-3=case .b(_):\n<#code#>\n}}
  case .a: break
  @unknown case _: break
  }

  switch payload {
  // expected-error@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.b(true)'}} {{+7:3-3=case .b(true):\n<#code#>\n}}
  // expected-note@-3 {{handle unknown values using "@unknown default"}} {{+7:3-3=@unknown default:\n<#fatalError()#>\n}}
  // expected-note@-4 {{add missing cases}} {{+7:3-3=case .b(true):\n<#code#>\n@unknown default:\n<#fatalError()#>\n}}
  case .a: break
  case .b(false): break
  }

  switch payload {
  // expected-warning@-1 {{switch must be exhaustive}}
  // expected-note@-2 {{add missing case: '.b(true)'}} {{+5:3-3=case .b(true):\n<#code#>\n}}
  case .a: break
  case .b(false): break
  @unknown case _: break
  }
}
