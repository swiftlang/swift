// RUN: %target-typecheck-verify-swift -swift-version 5

open class AnOpenClass {
  open let openLet: Int = 1 // expected-error {{'let' properties are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
  open static func test() {} // expected-error {{static declarations are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
}

final public class NonOpenClass {
  open func test() {} // expected-error {{members of 'final' classes are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
  open static func test() {} // expected-error {{static declarations are implicitly 'final'; use 'public' instead of 'open'}} {{3-7=public}}
}
