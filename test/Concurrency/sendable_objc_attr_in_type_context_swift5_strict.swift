// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 5 \
// RUN:   -strict-concurrency=complete \
// RUN:   -enable-experimental-feature SendableCompletionHandlers \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop
// REQUIRES: swift_feature_SendableCompletionHandlers

//--- Test.h
#define SWIFT_SENDABLE __attribute__((__swift_attr__("@Sendable")))
#define NONSENDABLE __attribute__((__swift_attr__("@_nonSendable")))
#define MAIN_ACTOR __attribute__((__swift_attr__("@MainActor")))

#pragma clang assume_nonnull begin

@import Foundation;

@interface MyValue : NSObject
@end

SWIFT_SENDABLE
@protocol P <NSObject>
@end

@interface SendableValue : NSObject<P>
@end

SWIFT_SENDABLE
@interface SendableMyValue : MyValue
@end

typedef void (^CompletionHandler)(void (^ SWIFT_SENDABLE)(void)) SWIFT_SENDABLE;

@interface Test : NSObject
-(void) makeRequest:
          (NSString *)typeIdentifier
          loadHandler:(void (^SWIFT_SENDABLE )(void (SWIFT_SENDABLE ^)(void)))loadHandler;
-(void) withSendableId: (void (^)(SWIFT_SENDABLE id)) handler;
-(void) withSendableCustom: (void (^)(MyValue *_Nullable SWIFT_SENDABLE)) handler;
-(void) withNonSendable:(NSString *)operation completionHandler:(void (^ _Nullable NONSENDABLE)(NSString *_Nullable, NSError * _Nullable)) handler;
-(void) withAliasCompletionHandler:(CompletionHandler)handler;
-(void) withMainActorId: (void (MAIN_ACTOR ^)(id)) handler;
@end

// Placement of SWIFT_SENDABLE matters here
void doSomethingConcurrently(__attribute__((noescape)) void SWIFT_SENDABLE (^block)(void));

@interface TestWithSendableID<T: SWIFT_SENDABLE id> : NSObject
-(void) add: (T) object;
@end

@interface TestWithSendableSuperclass<T: MyValue *SWIFT_SENDABLE> : NSObject
-(void) add: (T) object;
@end

@protocol InnerSendableTypes
-(void) testComposition:(SWIFT_SENDABLE MyValue *)composition;
-(void) test:(NSDictionary<NSString *, SWIFT_SENDABLE id> *)options;
-(void) testWithCallback:(NSString *)name handler:(MAIN_ACTOR void (^)(NSDictionary<NSString *, SWIFT_SENDABLE id> *, NSError * _Nullable))handler;
@end

@interface SwiftImpl : NSObject
-(id)initWithCallback:  (void (^ SWIFT_SENDABLE)(void)) callback;
-(void)computeWithCompletionHandler: (void (^)(void)) handler;
@end

#pragma clang assume_nonnull end

//--- main.swift

do {
  class SubTestNoActor : Test {
    @objc override func withMainActorId(_: @escaping (Any) -> Void) {}
    // expected-warning@-1 {{declaration 'withMainActorId' has a type with different global actor isolation from any potential overrides; this is an error in the Swift 6 language mode}}
  }

  class SubTestWithActor : Test {
    @objc override func withMainActorId(_: @MainActor @escaping (Any) -> Void) {} // Ok
  }
}

func test_sendable_attr_in_type_context(test: Test) {
  let fn: (String?, (any Error)?) -> Void = { _,_ in }

  test.withNonSendable("", completionHandler: fn) // Ok

  test.makeRequest("id") {
    doSomethingConcurrently($0) // Ok
  }

  test.makeRequest("id") { callback in
    _ = { @Sendable in
      callback() // Ok
    }
  }

  test.withSendableId { id in
    _ = { @Sendable in
      print(id) // Ok
    }
  }

  test.withSendableCustom { val in
    _ = { @Sendable in
      print(val!)
    }
  }

  let _: (@escaping @Sendable (@escaping @Sendable () -> Void) -> Void) -> Void = test.withAliasCompletionHandler

  test.withAliasCompletionHandler { callback in
    doSomethingConcurrently(callback) // Ok
  }

  _ = TestWithSendableID<SendableValue>() // Ok

  // TODO(diagnostics): Duplicate diagnostics
  TestWithSendableID().add(MyValue())
  // expected-warning@-1 3 {{type 'MyValue' does not conform to the 'Sendable' protocol}}

  TestWithSendableSuperclass().add(SendableMyValue()) // Ok

  // TODO(diagnostics): Duplicate diagnostics
  TestWithSendableSuperclass().add(MyValue())
  // expected-warning@-1 3 {{type 'MyValue' does not conform to the 'Sendable' protocol}}
}

class TestConformanceWithStripping : InnerSendableTypes {
  // expected-error@-1 {{type 'TestConformanceWithStripping' does not conform to protocol 'InnerSendableTypes'}}
  // expected-note@-2 {{add stubs for conformance}}

  func testComposition(_: MyValue) {
    // expected-note@-1 {{candidate has non-matching type '(MyValue) -> ()'}}
  }

  func test(_ options: [String: Any]) {
    // expected-note@-1 {{candidate has non-matching type '([String : Any]) -> ()'}}
  }

  func test(withCallback name: String, handler: @escaping @MainActor ([String : Any], (any Error)?) -> Void) {
    // expected-note@-1 {{candidate has non-matching type '(String, @escaping @MainActor ([String : Any], (any Error)?) -> Void) -> ()'}}
  }
}

class TestConformanceWithoutStripping : InnerSendableTypes {
  func testComposition(_: MyValue & Sendable) { // Ok
  }

  func test(_ options: [String: any Sendable]) { // Ok
  }

  func test(withCallback name: String, handler: @escaping @MainActor ([String : any Sendable], (any Error)?) -> Void) { // Ok
  }
}

@objc @implementation extension SwiftImpl {
  @objc public required init(callback: @escaping () -> Void) {}
  // expected-error@-1 {{initializer 'init(callback:)' should not be 'required' to match initializer declared by the header}}
  
  @objc func compute(completionHandler: @escaping () -> Void) {}
  // expected-warning@-1 {{sendability of function types in instance method 'compute(completionHandler:)' of type '(@escaping () -> Void) -> ()' does not match type '(@escaping @Sendable () -> Void) -> Void' declared by the header}}
}
