// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/Test.swiftmodule -emit-module-interface-path %t/Test.swiftinterface -module-name Test -enable-experimental-concurrency %s
// RUN: %FileCheck %s < %t/Test.swiftinterface
// RUN: %FileCheck %s -check-prefix SYNTHESIZED < %t/Test.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface -module-name Test %t/Test.swiftinterface

// RUN: %target-swift-frontend -emit-module -o /dev/null -merge-modules %t/Test.swiftmodule -disable-objc-attr-requires-foundation-module -emit-module-interface-path %t/TestFromModule.swiftinterface -module-name Test -enable-experimental-concurrency
// RUN: %FileCheck %s < %t/TestFromModule.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface -module-name Test %t/TestFromModule.swiftinterface

// REQUIRES: concurrency

// CHECK: public actor SomeActor
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public actor SomeActor {
  nonisolated func maine() { }
}

// CHECK: @globalActor public struct SomeGlobalActor
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
@globalActor
public struct SomeGlobalActor {
  public static let shared = SomeActor()
}

// CHECK: @{{(Test.)?}}SomeGlobalActor public protocol P1
// CHECK-NEXT: @{{(Test.)?}}SomeGlobalActor func method()
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
@SomeGlobalActor
public protocol P1 {
  func method()
}

// CHECK: class C1
// CHECK-NEXT: @{{(Test.)?}}SomeGlobalActor public func method()
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public class C1: P1 {
  public func method() { }
}

@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
@SomeGlobalActor
public class C2 { }

// CHECK: @{{(Test.)?}}SomeGlobalActor public class C2
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public class C3: C2 { }

// CHECK: public class C4 : Swift.UnsafeSendable
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public class C4: UnsafeSendable { }

// CHECK: public class C5 : @unchecked Swift.Sendable
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public class C5: @unchecked Sendable { }

@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public class C6 { }

// CHECK: extension {{(Test.)?}}C6 : @unchecked Swift.Sendable
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
extension C6: @unchecked Sendable { }

@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public class C7 { }

// CHECK: extension {{(Test.)?}}C7 : Swift.UnsafeSendable
@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
extension C7: UnsafeSendable { }

@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
public protocol P2 {
  @SomeGlobalActor func method()
}

@available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *)
// CHECK: class {{(Test.)?}}C8 : {{(Test.)?}}P2 {
public class C8 : P2 {
  // CHECK: @{{(Test.)?}}SomeGlobalActor public func method()
  public func method() {}
}

// FIXME: Work around a bug where module printing depends on the "synthesized"
// bit in conformances which is not serialized and not present in the textual
// form.

// SYNTHESIZED: extension Test.C2 : Swift.Sendable {}
