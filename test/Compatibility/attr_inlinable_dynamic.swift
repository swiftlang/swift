// RUN: %target-swift-frontend -typecheck %s -swift-version 4 -enable-objc-interop -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -typecheck %s -swift-version 4 -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-testing
// RUN: %target-swift-frontend -typecheck %s -swift-version 4.2 -enable-objc-interop -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -typecheck %s -swift-version 4.2 -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-testing

// RUN: %target-swift-frontend -typecheck %s -swift-version 4 -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-resilience -verify
// RUN: %target-swift-frontend -typecheck %s -swift-version 4 -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-testing -enable-resilience -verify
// RUN: %target-swift-frontend -typecheck %s -swift-version 4.2 -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-resilience -verify
// RUN: %target-swift-frontend -typecheck %s -swift-version 4.2 -enable-objc-interop -disable-objc-attr-requires-foundation-module -enable-testing -enable-resilience -verify

@objc internal enum InternalEnum: UInt8 {
  case dummy
}

public class Foo {
  @objc dynamic func dynamicFunc() -> InternalEnum {} // expected-note {{instance method 'dynamicFunc()' is not '@usableFromInline' or public}}
  @inlinable func inlineMe() {
    _ = dynamicFunc() // expected-error {{instance method 'dynamicFunc()' is internal and cannot be referenced from an '@inlinable' function}}
  }
}
