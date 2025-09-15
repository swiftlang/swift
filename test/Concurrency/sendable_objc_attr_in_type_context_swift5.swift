// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 5 \
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

@interface Shadowing : NSObject
-(void)computeWithCompletion: (void (^)(void)) completion;
-(void)updateWithCompletionHandler: (void (^_Nullable)(void)) handler;
@end

@protocol TestWitnesses
-(void)willSendDataWithCompletion: (void (^)(void)) completion;
@end

@interface DataHandler : NSObject
+(void)sendDataWithCompletion: (void (^)(MyValue *)) completion;
@end

@interface TestDR : NSObject
-(void) testWithCompletion: (void (^)(void)) completion;
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

  TestWithSendableID().add(MyValue())

  TestWithSendableSuperclass().add(SendableMyValue()) // Ok

  TestWithSendableSuperclass().add(MyValue())
}

class TestConformanceWithStripping : InnerSendableTypes {
  func testComposition(_: MyValue) {
    // expected-warning@-1 {{sendability of function types in instance method 'testComposition' does not match requirement in protocol 'InnerSendableTypes'; this is an error in the Swift 6 language mode}}
  }

  func test(_ options: [String: Any]) {
    // expected-warning@-1 {{sendability of function types in instance method 'test' does not match requirement in protocol 'InnerSendableTypes'; this is an error in the Swift 6 language mode}}
  }

  func test(withCallback name: String, handler: @escaping @MainActor ([String : Any], (any Error)?) -> Void) { // Ok
    // expected-warning@-1 {{sendability of function types in instance method 'test(withCallback:handler:)' does not match requirement in protocol 'InnerSendableTypes'; this is an error in the Swift 6 language mode}}
  }
}

class TestConformanceWithoutStripping : InnerSendableTypes {
  func testComposition(_: any MyValue & Sendable) { // Ok
  }

  func test(_ options: [String: any Sendable]) { // Ok
  }

  func test(withCallback name: String, handler: @escaping @MainActor ([String : any Sendable], (any Error)?) -> Void) { // Ok
  }
}

@objc @implementation extension SwiftImpl {
  @objc public required init(callback: @escaping () -> Void) {}
  // expected-error@-1 {{initializer 'init(callback:)' should not be 'required' to match initializer declared by the header}}
  
  @objc func compute(completionHandler: @escaping () -> Void) {} // Ok (no warnings with minimal checking)
}

// Methods deliberately has no `@Sendable` to make sure that
// shadowing rules are preserved when SendableCompletionHandlers feature is enabled.
@objc extension Shadowing {
  @objc func compute(completion: @escaping () -> Void) {}

  @objc func update(completionHandler: (() -> Void)? = nil) {}

  func testCompute() {
    self.compute { } // Ok - no ambiguity
    self.update { }  // Ok - no ambiguity
  }
}

@objc
class ImplicitShadowing : NSObject, TestWitnesses {
  func willSendData(completion: @escaping () -> Void) {}

  func test() {
    (self as AnyObject).willSendData { } // Ok
  }
}

protocol CompletionWithoutSendable {
  associatedtype T
  static func sendData(completion: @escaping (T) -> Void) // expected-note {{expected sendability to match requirement here}}
}

extension DataHandler : CompletionWithoutSendable {
  // expected-warning@-1 {{sendability of function types in class method 'sendData(completion:)' does not match requirement in protocol 'CompletionWithoutSendable'}}
  // It should be possible to infer `T` from method that mismatches on @Sendable in Swift 5 mode
}

extension TestDR {
  @_dynamicReplacement(for: test(completion:))
  func __replaceObjCFunc(_: @escaping () -> Void) {} // Ok
}

@MainActor
class InvalidIsolated: NSObject, @MainActor P {}
// expected-warning@-1 {{cannot form main actor-isolated conformance of 'InvalidIsolated' to SendableMetatype-inheriting protocol 'P'}}
