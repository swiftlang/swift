// RUN: %target-typecheck-verify-swift -swift-version 5

open class AnOpenClass {
  open let openLet: Int = 1 // expected-error {{'let' properties are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
}
