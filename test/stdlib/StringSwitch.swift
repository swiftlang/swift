// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var StringSwitchTests = TestSuite("StringSwitchTests")

func switchOver(_ s: String) -> Character {
  let (first, second) = ("first", "second")

  let ret1: Character
  switch s {
    case first: ret1 = "A"
    case second[...]: ret1 = "B"
    default: ret1 = "X"
  }

  let ret2: Character
  switch s[...] {
    case first: ret2 = "A"
    case second[...]: ret2 = "B"
    default: ret2 = "X"
  }

  expectEqual(ret1, ret2)
  return ret1
}

func switchOver<S1: StringProtocol, S2: StringProtocol>(
  _ s1: S1, _ s2: S2
) -> Character {
  let (first, second) = ("first", "second")

  // TODO(SR-12457): Enable
#if true
  fatalError()
#else
  let ret1: Character
  switch s1 {
    case first: ret1 = "A"
    case second[...]: ret1 = "B"
    case s2: ret2 = "="
    default: ret1 = "X"
  }

  let ret2: Character
  switch s2 {
    case first: ret1 = "A"
    case second[...]: ret1 = "B"
    case s1: ret2 = "="
    default: ret2 = "X"
  }

  expectEqual(ret1, ret2)
  return ret1
#endif
}

StringSwitchTests.test("switch") {
  let (first, second) = ("first", "second")
  let same = "same"
  let (foo, bar) = ("foo", "bar")

  expectEqual("A", switchOver(first))
  expectEqual("B", switchOver(second))
  expectEqual("X", switchOver(foo))

  // TODO(SR-12457): Enable
#if true
#else
  expectEqual("A", switchOver(first, first))
  expectEqual("B", switchOver(second, second))
  expectEqual("=", switchOver(same, same))
  expectEqual("X", switchOver(foo, bar))
  expectEqual("A", switchOver(first[...], first))
  expectEqual("B", switchOver(second[...], second))
  expectEqual("=", switchOver(same[...], same))
  expectEqual("X", switchOver(foo[...], bar))
#endif

}

runAllTests()
