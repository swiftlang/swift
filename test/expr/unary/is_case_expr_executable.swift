// RUN: %target-typecheck-verify-swift -disable-availability-checking

// REQUIRES: executable_test

enum Foo {
  case bar(Int)
  case baaz
}

let value = Foo.bar(10)
assert(value is case .bar)
assert(!(value is case .baaz))

let isBar10 = value is case .bar(10)
assert(isBar10)

let isNotBar12 = !(value is case .bar(12))
assert(isNotBar12)

// TODO: the assertions above work, but these hit an assertion in SILGen. why?
//assert(value is case .bar(10))
//assert(!(value is case .bar(12)))
