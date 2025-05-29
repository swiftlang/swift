// RUN: %target-swift-emit-silgen %s -verify

// Check that SILGen does not crash because of a parenthesized try expression
class Class {
  var num: Int = 1

  init() throws {}
}
_ = (try Class().num)
