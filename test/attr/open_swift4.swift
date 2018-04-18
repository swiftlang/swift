// RUN: %target-typecheck-verify-swift -swift-version 3
// RUN: %target-typecheck-verify-swift -swift-version 4

open class AnOpenClass {
  open let openLet: Int = 1 // expected-warning {{'let' properties are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
}
