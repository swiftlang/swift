// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-library-evolution

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

func testSwitch() -> Bool {
  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) { // expected-error {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.case11, _)'}}
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

  switch (OverlyLargeSpaceEnum.case1, OverlyLargeSpaceEnum.case2) { // expected-warning {{switch must be exhaustive}}
  // expected-note@-1 {{add missing case: '(.case11, _)'}}
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
  @unknown default: return false
  }

  // No diagnostic
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
  case (.case11, _): return true
  }
}
