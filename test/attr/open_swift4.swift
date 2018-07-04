// RUN: %target-typecheck-verify-swift -swift-version 3
// RUN: %target-typecheck-verify-swift -swift-version 4

open class AnOpenClass {
  open let openLet: Int = 1 // expected-warning {{'let' properties are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
  open static func test() {} // expected-warning {{static declarations are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
}

final public class NonOpenClass {
  open func test() {} // expected-warning {{members of 'final' classes are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
  open static func test() {} // expected-warning {{static declarations are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
}
